{-# LANGUAGE LambdaCase #-}
 
module Frontend.Chlorinate
	( chlorinate
	) where

import Control.Monad
import Control.Applicative
-- S for Src, D for Dest
import qualified Frontend.Program as S
import qualified Chloride.Program as D
import qualified Data.Map as M
import Success

chlorinate :: S.Program -> (Fail String) D.Program
chlorinate (S.Program funcs vars body) = do
	clMain <- do
		clBody <- chlorinateVB chlorinateName vars body
		let clFuncSig = D.FuncSig D.NameMain M.empty D.ClVoid
		return $ D.Func clFuncSig clBody []
	clFuncs <- mapM chlorinateFunc funcs
	return $ D.Program (clMain:clFuncs)

chlorinateVB nameHook (S.Vars vardecls) (S.Body statements) = do
	clVars <- mapM chlorinateVarDecl (splitVarDecls vardecls)
	clStatements <- mapM (chlorinateStatement nameHook) statements
	return $ D.Body (M.fromList clVars) clStatements

chlorinateFunc (S.Func name (S.Vars params) pasType vars body) = do
	let nameHook cs
		| cs == name = D.NameUnique <$> chlorinateName cs
		| otherwise  = chlorinateName cs
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
	let clFuncSig = D.FuncSig clName clParams clRetType
	return $ D.Func clFuncSig clBody [D.Access clRetName]

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
	S.PasLongInt -> return D.ClInteger
	S.PasReal    -> return D.ClDouble
	S.PasBoolean -> return D.ClBoolean
	S.PasString  -> return D.ClString
	S.PasType cs -> mzero

chlorinateMultiIf nameHook
	= \expr bodyThen mBodyElse -> do
		clLeaf
			<- (,)
			<$> chlorinateExpr nameHook expr
			<*> chlorinateVB nameHook (S.Vars []) bodyThen
		clBase <- case mBodyElse of
			Nothing -> return $ D.MultiIfBranch [] (D.Body M.empty [])
			Just (S.Body [S.IfBranch expr' bodyThen' mBodyElse'])
				-> chlorinateMultiIf nameHook expr' bodyThen' mBodyElse'
			Just body -> do
				clBody <- chlorinateVB nameHook (S.Vars []) body
				return $ D.MultiIfBranch [] clBody
		return $ clBase { D._multiIfLeafs = clLeaf : D._multiIfLeafs clBase }

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
		-> D.MultiIfStatement
		<$> chlorinateMultiIf nameHook expr bodyThen mBodyElse
	S.CaseBranch expr leafs mBodyElse
		-> D.CaseStatement <$> do
			clExpr <- chlorinateExpr nameHook expr
			let chlorinateLeaf (exprs, body) = do
				clExprs <- mapM (chlorinateExpr nameHook) exprs
				clBody <- chlorinateVB nameHook (S.Vars []) body
				return (clExprs, clBody)
			clLeafs <- mapM chlorinateLeaf leafs
			clBodyElse <- case mBodyElse of
				Just bodyElse ->
					chlorinateVB nameHook (S.Vars []) bodyElse
				Nothing ->
					return $ D.Body M.empty []
			return $ D.CaseBranch clExpr clLeafs clBodyElse


chlorinateArgument nameHook = \case
	S.Access name -> D.LValue <$> nameHook name
	expr -> D.RValue <$> chlorinateExpr nameHook expr

chlorinateExpr nameHook = \case
	S.Access name -> D.Access <$> nameHook name
	S.Call name exprs
		 -> D.Call
		<$> (D.CallName <$> chlorinateName name)
		<*> mapM (chlorinateExpr nameHook) exprs
	S.INumber intSection
		-> return
		 $ D.Primary
		 $ D.INumber intSection
	S.FNumber intSection fracSection
		-> return
		 $ D.Primary
		 $ D.FNumber intSection fracSection
	S.ENumber intSection fracSection eSign eSection
		-> return
		 $ D.Primary
		 $ D.ENumber intSection fracSection eSign eSection
	S.Quote cs -> return $ D.Primary (D.Quote cs)
	S.BTrue  -> return $ D.Primary D.BTrue
	S.BFalse -> return $ D.Primary D.BFalse
	S.Binary op x y
		 -> D.Call
		<$> (D.CallOperator <$> chlorinateOp op)
		<*> mapM (chlorinateExpr nameHook) [x, y]
	S.Unary op x -> case op of
		S.UOpPlus -> chlorinateExpr nameHook x
		S.UOpNegate
			 -> D.Call (D.CallOperator D.OpNegate)
			<$> mapM (chlorinateExpr nameHook) [x]

chlorinateOp = return . \case
	S.OpAdd -> D.OpAdd
	S.OpSubtract -> D.OpSubtract
	S.OpMultiply -> D.OpMultiply
	S.OpDivide -> D.OpDivide
	S.OpLess -> D.OpLess
	S.OpMore -> D.OpMore
	S.OpEquals -> D.OpEquals
	S.OpAnd -> D.OpAnd
	S.OpOr -> D.OpOr
	S.OpRange -> D.OpRange
