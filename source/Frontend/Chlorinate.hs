{-# LANGUAGE LambdaCase #-}
 
module Frontend.Chlorinate
	( chlorinate
	) where

import Control.Monad
import Control.Applicative
-- S for Src, D for Dest
import qualified Frontend.Program  as S
import qualified Chloride.Chloride as D
import qualified Data.Map as M

chlorinate :: S.Program -> Maybe D.Program
chlorinate (S.Program funcs vars body)
	= do
		clMain <- do
			clBody <- chlorinateVB vars body
			return $ D.Func
				D.NameMain
				M.empty
				D.ClVoid
				clBody
		clFuncs <- mapM
			chlorinateFunc funcs
		return $ D.Program (clMain:clFuncs)

chlorinateVB (S.Vars vardecls) (S.Body statements)
	= do
		clVars <- mapM chlorinateVarDecl vardecls
		clStatements <- mapM chlorinateStatement statements
		return $ D.Body (M.fromList clVars) clStatements

chlorinateFunc (S.Func name (S.Vars params) pasType vars body)
	 =  D.Func
	<$> chlorinateName name
	<*> (M.fromList <$> mapM chlorinateVarDecl params)
	<*> chlorinateType pasType
	<*> chlorinateVB vars body

chlorinateName name
	= return $ D.Name name

chlorinateVarDecl (S.VarDecl name pasType)
	 =  (,)
	<$> chlorinateName name
	<*> chlorinateType pasType

chlorinateType = \case
	S.PasInteger -> return D.ClInteger
	S.PasReal    -> return D.ClDouble
	S.PasBoolean -> return D.ClBoolean
	S.PasString  -> return D.ClString
	S.PasType cs -> mzero

chlorinateStatement = \case
	S.Assign name expr
		 -> D.Assign
		<$> chlorinateName name
		<*> chlorinateExpr expr
	S.Execute (name) exprs
		 -> D.Execute
		<$> chlorinateName name
		<*> mapM chlorinateArgument exprs
	S.ForCycle closure name fromExpr toExpr body
		 -> D.ForStatement
		<$> ( D.ForCycle
			<$> mapM chlorinateName closure
			<*> chlorinateName name
			<*> chlorinateExpr fromExpr
			<*> chlorinateExpr toExpr
			<*> chlorinateVB (S.Vars []) body
			)

chlorinateArgument = \case
	S.Access name -> D.LValue <$> chlorinateName name
	expr -> D.RValue <$> chlorinateExpr expr

chlorinateExpr = \case
	S.Access name
		 -> D.Access
		<$> chlorinateName name
	S.Call name exprs
		 -> D.Call
		<$> chlorinateName name
		<*> mapM chlorinateExpr exprs
	S.Number cs
		-> return
		 $ D.Primary
		 $ D.Number cs
	S.Quote cs
		-> return
		 $ D.Primary
		 $ D.Quote cs
	S.Binary op x y
		 -> D.Binary
		<$> chlorinateOp op
		<*> chlorinateExpr x
		<*> chlorinateExpr y

chlorinateOp = \case
	S.OpAdd -> return D.OpAdd
	S.OpSubtract -> return D.OpSubtract
	S.OpMultiply -> return D.OpMultiply
	S.OpDivide -> return D.OpDivide
