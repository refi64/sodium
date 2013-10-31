{-# LANGUAGE LambdaCase #-}

module Backend.Transform
	( transform
	) where

import Data.List (genericReplicate)
import Control.Monad (foldM)
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Frontend.Program as S
import qualified  Backend.Program as D

transform :: S.Program -> Maybe D.Program
transform (S.Program vars body) = do
	modBody <- transformBody (transformVars vars) Nothing body
	return $ D.Program [D.ValueDef "main" modBody] ["Control.Monad"]

data VarState = VarState D.HsType Integer
type VarStates = M.Map D.Name VarState

transformVars :: Maybe S.Vars -> VarStates
transformVars Nothing = M.empty
transformVars (Just (S.Vars vardecls))
	= M.fromList $ flip map vardecls $ \(S.VarDecl name pasType) ->
		(transformName name, VarState (transformType pasType) (-1))

transformName :: S.Name -> D.Name
transformName = id

transformType :: S.PasType -> D.HsType
transformType = \case
	S.PasInteger -> D.HsInteger
	S.PasReal -> D.HsDouble
	S.PasBoolean -> D.HsBoolean
	S.PasString -> D.HsString
	(S.PasType name) -> D.HsType (transformName name)

transformBody :: VarStates -> Maybe D.Name -> S.Body -> Maybe D.Expression
transformBody varStates mName (S.Body statements) = do
	(varStates', modStatements) <- foldM transformStatement (varStates, []) statements
	modStatement <- case mName of
		Nothing -> return $ D.DoExecute returnUnitStatement
		Just name -> do
			VarState t i <- M.lookup name varStates'
			return $ D.DoExecute (beta [D.Access "return", D.Access (unvzName name i)])
	return $ D.DoExpression (reverse (modStatement:modStatements))

transformStatement :: (VarStates, [D.DoStatement]) -> S.Statement -> Maybe (VarStates, [D.DoStatement])
transformStatement (varStates, modStatements) = \case
	S.Execute "readln" [S.Expression (S.Term (S.Access name))] -> do
		VarState t i <- M.lookup name varStates
		let j = succ i
		let varStates' = M.insert name (VarState t j) varStates
		let modStatement = D.DoBind
			(unvzName name j)
			(beta [D.Access "fmap", D.Access "read", D.Access "getLine"] `D.Typed` D.HsIO t)
		return (varStates', modStatement:modStatements)
	S.Execute name exprs -> do
		modExpr <- transformExecute varStates name exprs
		let modStatement = D.DoExecute modExpr
		return (varStates, modStatement:modStatements)
	S.Assign name expr -> do
		VarState t i <- M.lookup name varStates
		let j = succ i
		let varStates' = M.insert name (VarState t j) varStates
		modExpr <- transformExpr varStates expr
		let modStatement = D.DoLet (unvzName name j) modExpr
		return (varStates', modStatement:modStatements)
	S.ForCycle [accName] iterName exprFrom exprTo body -> do
		VarState t i <- M.lookup accName varStates
		let j = succ i
		let varStates' = M.insert accName (VarState t j) varStates
		modExprFrom <- transformExpr varStates exprFrom
		modExprTo <- transformExpr varStates exprTo
		let modRange = D.Range modExprFrom modExprTo
		let varStates''
			= M.insert accName (VarState t 0)
			$ M.insert iterName (VarState D.HsInteger 0)
			$ varStates
		modBody <- transformBody varStates'' (Just accName) body
		let modStatement = D.DoBind
			(unvzName accName j)
			(beta [D.Access "foldM", D.Lambda [accName, iterName] modBody, D.Access (unvzName accName i), modRange])
		return (varStates', modStatement:modStatements)

transformExecute :: VarStates -> S.Name -> [S.Expression] -> Maybe D.Expression
transformExecute varStates = \case
	"writeln" -> \case
		[] -> Just $ D.Beta (D.Access "putStrLn") (D.Quote "")
		exprs -> do
			modExprs <- mapM (transformExpr varStates) exprs
			Just
				$ D.Beta (D.Access "putStrLn")
				$ foldr1 (D.Binary "++")
				$ map (showNoString varStates)
				$ modExprs

beta = foldl1 D.Beta

unvzName name i = name ++ genericReplicate i '\''
unvzTable = M.fromList . map f . M.toList where
	f (name, VarState t i) = (unvzName name i, t)

showNoString :: VarStates -> D.Expression -> D.Expression
showNoString varStates = \case
	D.Access name -> case M.lookup name (unvzTable varStates) of
		Nothing -> undefined
		Just t -> case t of
			D.HsString -> D.Access name
			_ -> D.Beta (D.Access "show") (D.Access name)
	a -> a -- TODO: bypass any D.Expression to transform underlying D.Access

transformExpr :: VarStates -> S.Expression -> Maybe D.Expression
transformExpr varStates = \case
	S.Expression term -> transformTerm varStates term
	S.ExpressionAdd term expr -> do
		x <- transformTerm varStates term
		y <- transformExpr varStates expr
		return $ D.Binary "+" x y

transformTerm :: VarStates -> S.Term -> Maybe D.Expression
transformTerm varStates = \case
	S.Term prim -> transformPrim varStates prim
	S.TermMultiply term prim -> do
		x <- transformTerm varStates term
		y <- transformPrim varStates prim
		return $ D.Binary "*" x y

transformPrim :: VarStates -> S.Prim -> Maybe D.Expression
transformPrim varStates = \case
	S.Quote cs -> return $ D.Quote cs
	S.Number cs -> return $ D.Number cs
	S.Access name -> do
		VarState t i <- M.lookup name varStates
		return $ D.Access (unvzName name i)
	S.Enclosed expr -> transformExpr varStates expr

returnUnitStatement :: D.Expression
returnUnitStatement = D.Beta (D.Access "return") (D.Tuple [])
