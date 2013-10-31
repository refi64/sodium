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
	modBody <- transformBody (transformVars vars) body
	return $ D.Program [D.ValueDef "main" modBody]

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

transformBody :: VarStates -> S.Body -> Maybe D.Expression
transformBody varStates (S.Body statements)
	= transformStatements varStates statements

transformStatements :: VarStates -> [S.Statement] -> Maybe D.Expression
transformStatements varStates statements = do
	(varStates', modStatements) <- foldM transformStatement (varStates, []) statements
	let modStatements' = D.DoExecute returnUnitStatement : modStatements
	return $ D.DoExpression (reverse modStatements')

transformStatement :: (VarStates, [D.DoStatement]) -> S.Statement -> Maybe (VarStates, [D.DoStatement])
transformStatement (varStates, modStatements) = \case
	S.Execute "readln" [S.Expression (S.Term (S.Access name))] ->
		case M.lookup name varStates of
			Nothing -> Nothing
			Just (VarState t i) -> do
				let j = succ i
				let varStates' = M.insert name (VarState t j) varStates
				let modStatement = D.DoBind
					(unvzName name j)
					(beta [D.Access "fmap", D.Access "read", D.Access "getLine"] `D.Typed` D.HsIO t)
				Just (varStates', modStatement:modStatements)
	S.Execute name exprs -> do
		modExpr <- transformExecute varStates name exprs
		let modStatement = D.DoExecute modExpr
		Just (varStates, modStatement:modStatements)
	S.Assign name expr ->
		case M.lookup name varStates of
			Nothing -> Nothing
			Just (VarState t i) -> do
				let j = succ i
				let varStates' = M.insert name (VarState t j) varStates
				modExpr <- transformExpr varStates expr
				let modStatement = D.DoLet (unvzName name j) modExpr
				Just (varStates', modStatement:modStatements)

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
	S.Quote cs -> Just $ D.Quote cs
	S.Number cs -> Just $ D.Number cs
	S.Access name -> case M.lookup name varStates of
		Nothing -> Nothing
		Just (VarState t i) -> Just $ D.Access (unvzName name i)
	S.Enclosed expr -> transformExpr varStates expr

returnUnitStatement :: D.Expression
returnUnitStatement = D.Beta (D.Access "return") (D.Tuple [])
