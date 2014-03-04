{-# LANGUAGE LambdaCase #-}
 
module Frontend.Chlorinate
	( chlorinate
	) where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
-- S for Src, D for Dest
import qualified Frontend.Program as S
import qualified Chloride.Program as D
import qualified Data.Map as M
import Success

chlorinate :: S.Program -> (Fail String) D.Program
chlorinate program = runReaderT (chlor program) []

chlor :: S.Program -> ReaderT [S.Name] (Fail String) D.Program
chlor (S.Program funcs vars body) = do
	clMain <- do
		clBody <- chlorinateVB vars body
		let clFuncSig = D.FuncSig D.NameMain M.empty D.ClVoid
		return $ D.Func clFuncSig clBody []
	clFuncs <- mapM chlorinateFunc funcs
	return $ D.Program (clMain:clFuncs)

chlorinateVB (S.Vars vardecls) (S.Body statements) = do
	clVars <- mapM chlorinateVarDecl (splitVarDecls vardecls)
	clStatements <- mapM chlorinateStatement statements
	return $ D.Body (M.fromList clVars) clStatements

nameHook cs = do
	names <- ask
	let wrap = if cs `elem` names then D.NameUnique else id
	wrap <$> chlorinateName cs

chlorinateFunc (S.Func name (S.Vars params) pasType vars body)
	= withReaderT (name:) $ do
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
		clBody <- enclose <$> chlorinateVB vars body
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

chlorinateMultiIf
	= \expr bodyThen mBodyElse -> do
		clLeaf
			<- (,)
			<$> chlorinateExpr expr
			<*> chlorinateVB (S.Vars []) bodyThen
		clBase <- case mBodyElse of
			Nothing -> return $ D.MultiIfBranch [] (D.Body M.empty [])
			Just (S.Body [S.IfBranch expr' bodyThen' mBodyElse'])
				-> chlorinateMultiIf expr' bodyThen' mBodyElse'
			Just body -> do
				clBody <- chlorinateVB (S.Vars []) body
				return $ D.MultiIfBranch [] clBody
		return $ clBase { D._multiIfLeafs = clLeaf : D._multiIfLeafs clBase }

chlorinateStatement = \case
	S.Assign name expr
		 -> D.Assign
		<$> nameHook name
		<*> chlorinateExpr expr
	S.Execute name exprs
		 -> D.Execute
		<$> (D.ExecuteName <$> chlorinateName name)
		<*> mapM chlorinateArgument exprs
	S.ForCycle name fromExpr toExpr body
		-> D.ForStatement <$> do
			clName <- nameHook name
			clFromExpr <- chlorinateExpr fromExpr
			clToExpr <- chlorinateExpr toExpr
			clBody <- chlorinateVB (S.Vars []) body
			return $ D.ForCycle clName clFromExpr clToExpr clBody
	S.IfBranch expr bodyThen mBodyElse
		-> D.MultiIfStatement
		<$> chlorinateMultiIf expr bodyThen mBodyElse
	S.CaseBranch expr leafs mBodyElse
		-> D.CaseStatement <$> do
			clExpr <- chlorinateExpr expr
			let chlorinateLeaf (exprs, body) = do
				clExprs <- mapM chlorinateExpr exprs
				clBody <- chlorinateVB (S.Vars []) body
				return (clExprs, clBody)
			clLeafs <- mapM chlorinateLeaf leafs
			clBodyElse <- case mBodyElse of
				Just bodyElse ->
					chlorinateVB (S.Vars []) bodyElse
				Nothing ->
					return $ D.Body M.empty []
			return $ D.CaseBranch clExpr clLeafs clBodyElse


chlorinateArgument = \case
	S.Access name -> D.LValue <$> nameHook name
	expr -> D.RValue <$> chlorinateExpr expr

chlorinateExpr = \case
	S.Access name -> D.Access <$> nameHook name
	S.Call name exprs
		 -> D.Call
		<$> (D.CallName <$> chlorinateName name)
		<*> mapM chlorinateExpr exprs
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
		<*> mapM chlorinateExpr [x, y]
	S.Unary op x -> case op of
		S.UOpPlus -> chlorinateExpr x
		S.UOpNegate
			 -> D.Call (D.CallOperator D.OpNegate)
			<$> mapM chlorinateExpr [x]

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
