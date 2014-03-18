{-# LANGUAGE FlexibleInstances #-}
 
module Sodium.Frontend.Chlorinate (chlorinate) where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Lens
-- S for Src, D for Dest
import qualified Sodium.Frontend.Program as S
import qualified Sodium.Chloride.Program as D
import qualified Data.Map as M
import Sodium.Success

chlorinate :: S.Program -> (Fail String) D.Program
chlorinate = flip runReaderT [] . chlor

type ChlorEnv = [S.Name]

class Chlor s d | s -> d where
	chlor :: s -> ReaderT ChlorEnv (Fail String) d

instance Chlor S.Program D.Program where
	chlor (S.Program funcs vars body) = do
		clMain <- do
			clBody <- chlor (VB vars body)
			let clFuncSig = D.FuncSig D.NameMain M.empty D.ClVoid
			return $ D.Func clFuncSig clBody []
		clFuncs <- mapM chlor funcs
		return $ D.Program (clMain:clFuncs)

data VB = VB S.Vars S.Body
vb = VB (S.Vars [])

instance Chlor VB D.Body where
	chlor (VB (S.Vars vardecls) (S.Body statements)) = do
		clVars <- mapM chlor (splitVarDecls vardecls)
		clStatements <- mapM chlor statements
		return $ D.Body (M.fromList clVars) clStatements

nameHook cs = do
	names <- ask
	let wrap = if cs `elem` names then D.NameUnique else id
	wrap <$> chlor cs

instance Chlor S.Func D.Func where
	chlor (S.Func name (S.Vars params) pasType vars body)
		= withReaderT (name:) $ do
			clName <- chlor name
			clParams <- M.fromList <$> mapM chlor (splitVarDecls params)
			clRetType <- chlor pasType
			clRetName <- nameHook name
			let enclose = D.bodyVars
				%~ flip M.union (M.singleton clRetName clRetType)
			clBody <- enclose <$> chlor (VB vars body)
			let clFuncSig = D.FuncSig clName clParams clRetType
			return $ D.Func clFuncSig clBody [D.Access clRetName]

instance Chlor S.Name D.Name where
	chlor = return . D.Name

splitVarDecls vardecls = do
	S.VarDecl names t <- vardecls
	name <- names
	return $ VarDecl name t

data VarDecl = VarDecl S.Name S.PasType

instance Chlor VarDecl (D.Name, D.ClType) where
	chlor (VarDecl name pasType)
		 = (,) <$> chlor name <*> chlor pasType

instance Chlor S.PasType D.ClType where
	chlor = \case
		S.PasInteger -> return D.ClInteger
		S.PasLongInt -> return D.ClInteger
		S.PasReal    -> return D.ClDouble
		S.PasBoolean -> return D.ClBoolean
		S.PasString  -> return D.ClString
		S.PasType cs -> mzero

chlorinateMultiIf expr bodyThen mBodyElse = do
	clLeaf <- (,) <$> chlor expr <*> chlor (vb bodyThen)
	clBase <- case mBodyElse of
		Nothing -> return $ D.MultiIfBranch [] (D.Body M.empty [])
		Just (S.Body [S.IfBranch expr' bodyThen' mBodyElse'])
			-> chlorinateMultiIf expr' bodyThen' mBodyElse'
		Just body -> do
			clBody <- chlor (vb body)
			return $ D.MultiIfBranch [] clBody
	return $ D.multiIfLeafs %~ (clLeaf:) $ clBase

instance Chlor S.Statement D.Statement where
	chlor = \case
		S.Assign name expr -> D.Assign <$> nameHook name <*> chlor expr
		S.Execute name exprs
			 -> D.Execute
			<$> (D.ExecuteName <$> chlor name)
			<*> mapM chlor (map Argument exprs)
		S.ForCycle name fromExpr toExpr body
			-> D.ForStatement <$> do
				clName <- nameHook name
				clFromExpr <- chlor fromExpr
				clToExpr <-chlor toExpr
				clBody <- chlor (vb body)
				return $ D.ForCycle clName clFromExpr clToExpr clBody
		S.IfBranch expr bodyThen mBodyElse
			-> D.MultiIfStatement
			<$> chlorinateMultiIf expr bodyThen mBodyElse
		S.CaseBranch expr leafs mBodyElse
			-> D.CaseStatement <$> do
				clExpr <- chlor expr
				let chlorLeaf (exprs, body) = do
					clExprs <- mapM chlor exprs
					clBody <- chlor (vb body)
					return (clExprs, clBody)
				clLeafs <- mapM chlorLeaf leafs
				clBodyElse <- case mBodyElse of
					Just bodyElse ->
						chlor (vb bodyElse)
					Nothing ->
						return $ D.Body M.empty []
				return $ D.CaseBranch clExpr clLeafs clBodyElse

data Argument = Argument S.Expression

instance Chlor Argument D.Argument where
	chlor (Argument expr) = case expr of
		S.Access name -> D.LValue <$> nameHook name
		expr -> D.RValue <$> chlor expr

instance Chlor S.Expression D.Expression where
	chlor = \case
		S.Access name -> D.Access <$> nameHook name
		S.Call name exprs
			 -> D.Call
			<$> (D.CallName <$> chlor name)
			<*> mapM chlor exprs
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
			<$> (D.CallOperator <$> chlor op)
			<*> mapM chlor [x, y]
		S.Unary op x -> case op of
			S.UOpPlus -> chlor x
			S.UOpNegate
				 -> D.Call (D.CallOperator D.OpNegate)
				<$> mapM chlor [x]

instance Chlor S.Operator D.Operator where
	chlor = return . \case
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
