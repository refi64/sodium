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
import Success

chlorinate :: S.Program -> (Fail String) D.Program
chlorinate (S.Program funcs vars body)
	= do
		clMain <- do
			clBody <- chlorinateVB chlorinateName vars body
			return $ D.Func
				(D.FuncSig D.NameMain M.empty D.ClVoid) clBody []
		clFuncs <- mapM
			chlorinateFunc funcs
		return $ D.Program (clMain:clFuncs)

chlorinateVB nameHook (S.Vars vardecls) (S.Body statements)
	= do
		clVars <- mapM chlorinateVarDecl (splitVarDecls vardecls)
		clStatements <- mapM (chlorinateStatement nameHook) statements
		return $ D.Body (M.fromList clVars) clStatements

chlorinateFunc (S.Func name (S.Vars params) pasType vars body)
	 = do
		clName <- chlorinateName name
		clParams <- M.fromList <$> mapM chlorinateVarDecl (splitVarDecls params)
		clRetType <- chlorinateType pasType
		clRetName <- nameHook name
		let enclose body
			= body
			{ D._bodyVars = M.union
				(D._bodyVars body)
				(M.singleton clRetName clRetType)
			}
		clBody <- enclose <$> chlorinateVB nameHook vars body
		return $ D.Func (D.FuncSig clName clParams clRetType) clBody [D.Access clRetName]
	where
		nameHook cs
			| cs == name = D.NameUnique <$> chlorinateName cs
			| otherwise  = chlorinateName cs

chlorinateName name
	= return $ D.Name name

splitVarDecls vardecls = do
	S.VarDecl names t <- vardecls
	name <- names
	return (name, t)

chlorinateVarDecl (name, pasType)
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
		<$> (D.ExecuteName <$> chlorinateName name)
		<*> mapM (chlorinateArgument nameHook) exprs
	S.ForCycle name fromExpr toExpr body
		-> D.ForStatement <$> do
			clName <- nameHook name
			clFromExpr <- chlorinateExpr nameHook fromExpr
			clToExpr <- chlorinateExpr nameHook toExpr
			clBody <- chlorinateVB nameHook (S.Vars []) body
			return $ D.ForCycle clName clFromExpr clToExpr clBody
	S.IfBranch expr bodyThen mBodyElse
		-> D.IfStatement <$> do
			clExpr <- chlorinateExpr nameHook expr
			clBodyThen <- chlorinateVB nameHook (S.Vars []) bodyThen
			clBodyElse <- case mBodyElse of
				Just bodyElse ->
					chlorinateVB nameHook (S.Vars []) bodyElse
				Nothing ->
					return $ D.Body M.empty []
			return $ D.IfBranch clExpr clBodyThen clBodyElse


chlorinateArgument nameHook = \case
	S.Access name -> D.LValue <$> nameHook name
	expr -> D.RValue <$> chlorinateExpr nameHook expr

chlorinateExpr nameHook = \case
	S.Access name
		 -> D.Access
		<$> nameHook name
	S.Call name exprs
		 -> D.Call
		<$> (D.CallName <$> chlorinateName name)
		<*> mapM (chlorinateExpr nameHook) exprs
	S.Number cs
		-> return
		 $ D.Primary
		 $ D.Number cs
	S.Quote cs
		-> return
		 $ D.Primary
		 $ D.Quote cs
	S.BTrue
		-> return
		 $ D.Primary D.BTrue
	S.BFalse
		-> return
		 $ D.Primary D.BFalse
	S.Binary op x y
		 -> D.Call
		<$> (D.CallOperator <$> chlorinateOp op)
		<*> mapM (chlorinateExpr nameHook) [x, y]

chlorinateOp = \case
	S.OpAdd -> return D.OpAdd
	S.OpSubtract -> return D.OpSubtract
	S.OpMultiply -> return D.OpMultiply
	S.OpDivide -> return D.OpDivide
	S.OpLess -> return D.OpLess
	S.OpMore -> return D.OpMore
	S.OpEquals -> return D.OpEquals
