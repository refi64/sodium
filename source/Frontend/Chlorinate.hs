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
			clBody <- chlorinateVB chlorinateName vars body
			return $ D.Func
				D.NameMain
				M.empty
				D.ClVoid
				D.NameMain
				clBody
		clFuncs <- mapM
			chlorinateFunc funcs
		return $ D.Program (clMain:clFuncs)

chlorinateVB nameHook (S.Vars vardecls) (S.Body statements)
	= do
		clVars <- mapM chlorinateVarDecl vardecls
		clStatements <- mapM (chlorinateStatement nameHook) statements
		return $ D.Body (M.fromList clVars) clStatements

chlorinateFunc (S.Func name (S.Vars params) pasType vars body)
	 =  D.Func
	<$> chlorinateName name
	<*> (M.fromList <$> mapM chlorinateVarDecl params)
	<*> chlorinateType pasType
	<*> nameHook name
	<*> chlorinateVB nameHook vars body
	where
		nameHook cs =
			if cs == name
				then D.NameUnique <$> chlorinateName cs
				else chlorinateName cs

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

chlorinateStatement nameHook = \case
	S.Assign name expr
		 -> D.Assign
		<$> nameHook name
		<*> chlorinateExpr nameHook expr
	S.Execute name exprs
		 -> D.Execute
		<$> chlorinateName name
		<*> mapM (chlorinateArgument nameHook) exprs
	S.ForCycle closure name fromExpr toExpr body
		 -> D.ForStatement
		<$> ( D.ForCycle
			<$> mapM nameHook closure
			<*> nameHook name
			<*> chlorinateExpr nameHook fromExpr
			<*> chlorinateExpr nameHook toExpr
			<*> chlorinateVB nameHook (S.Vars []) body
			)

chlorinateArgument nameHook = \case
	S.Access name -> D.LValue <$> nameHook name
	expr -> D.RValue <$> chlorinateExpr nameHook expr

chlorinateExpr nameHook = \case
	S.Access name
		 -> D.Access
		<$> nameHook name
	S.Call name exprs
		 -> D.Call
		<$> chlorinateName name
		<*> mapM (chlorinateExpr nameHook) exprs
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
		<*> chlorinateExpr nameHook x
		<*> chlorinateExpr nameHook y

chlorinateOp = \case
	S.OpAdd -> return D.OpAdd
	S.OpSubtract -> return D.OpSubtract
	S.OpMultiply -> return D.OpMultiply
	S.OpDivide -> return D.OpDivide
