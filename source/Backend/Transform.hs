{-# LANGUAGE LambdaCase #-}

module Backend.Transform
	( transform
	) where

import Data.List (genericReplicate, mapAccumL)
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Frontend.Program as S
import qualified  Backend.Program as D

transform :: S.Program -> D.Program
transform (S.Program vars body) =
	D.Program [D.ValueDef "main" (transformBody (transformVars vars) body)]

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

transformBody :: VarStates -> S.Body -> D.Expression
transformBody varStates (S.Body statements)
	= transformStatements varStates statements

transformStatements :: VarStates -> [S.Statement] -> D.Expression
transformStatements varStates statements
	= D.DoExpression
	$ (++ [D.DoExecute returnUnitStatement])
	$ snd
	$ mapAccumL transformStatement varStates statements

transformStatement :: VarStates -> S.Statement -> (VarStates, D.DoStatement)
transformStatement varStates = \case
	S.Execute "readln" [S.Expression (S.Term (S.Access name))] ->
		case M.lookup name varStates of
			Nothing -> error "TODO: Maybe monad to handle errors (READLN)"
			Just (VarState t i) -> let j = succ i in
				( M.insert name (VarState t j) varStates
				, D.DoBind
					(unvzName name j)
					(beta [D.Access "fmap", D.Access "read", D.Access "getLine"] `D.Typed` D.HsIO t)
				)
	S.Execute name exprs ->
		( varStates
		, D.DoExecute $ transformExecute varStates name exprs
		)
	S.Assign name expr ->
		case M.lookup name varStates of
			Nothing -> error "TODO: Maybe monad to handle errors (ASSIGN)"
			Just (VarState t i) -> let j = succ i in
				( M.insert name (VarState t j) varStates
				, D.DoLet
					(unvzName name j)
					(transformExpr varStates expr)
				)

transformExecute :: VarStates -> S.Name -> [S.Expression] -> D.Expression
transformExecute varStates = \case
	"writeln" -> \case
		[] -> D.Beta (D.Access "putStrLn") (D.Quote "")
		exprs -> D.Beta (D.Access "putStrLn")
			$ foldr1 (D.Binary "++")
			$ map (showNoString varStates)
			$ map (transformExpr varStates)
			$ exprs

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

transformExpr :: VarStates -> S.Expression -> D.Expression
transformExpr varStates = \case
	S.Expression term -> transformTerm varStates term
	S.ExpressionAdd term expr -> D.Binary "+"
		(transformTerm varStates term)
		(transformExpr varStates expr)

transformTerm :: VarStates -> S.Term -> D.Expression
transformTerm varStates = \case
	S.Term prim -> transformPrim varStates prim
	S.TermMultiply term prim -> D.Binary "*"
		(transformTerm varStates term)
		(transformPrim varStates prim)

transformPrim :: VarStates -> S.Prim -> D.Expression
transformPrim varStates = \case
	S.Quote cs -> D.Quote cs
	S.Number cs -> D.Number cs
	S.Access name -> case M.lookup name varStates of
		Nothing -> error "TODO: Maybe monad to handle errors (PRIM)"
		Just (VarState t i) -> D.Access (unvzName name i)
	S.Enclosed expr -> transformExpr varStates expr

returnUnitStatement :: D.Expression
returnUnitStatement = D.Beta (D.Access "return") (D.Tuple [])
