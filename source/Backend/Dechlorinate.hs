{-# LANGUAGE LambdaCase #-}

module Backend.Dechlorinate
	( dechlorinate
	) where

import Data.List (genericReplicate)
import Control.Monad (forM, foldM, guard)
import Control.Applicative ((<$>))
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Chloride.Chloride as S
import qualified  Backend.Program  as D

dechlorinate = transform

transform :: S.Program -> Maybe D.Program
transform (S.Program funcs) = do
	funcDefs <- mapM transformFunc funcs
	return $ D.Program funcDefs ["Control.Monad"]

data VarState = VarState D.HsType Integer
type VarStates = M.Map D.Name VarState

initVarState name clType =
	( transformName name
	, VarState (transformType clType) (-1)
	)

transformVars :: S.Vars -> VarStates
transformVars
	= M.fromList
	. map (uncurry initVarState)
	. M.toList

-- WARNING: Name handling is broken. Beware, because
-- strange errors may appear in generated code.
-- TODO: Name management system
transformName :: S.Name -> D.Name
transformName = \case
	S.NameMain -> "main"
	S.Name cs  -> cs

hookName :: D.Name -> D.Name -> D.Name
hookName u v = if u == v then v ++ "_" else v

transformType :: S.ClType -> D.HsType
transformType = \case
	S.ClInteger -> D.HsType "Integer"
	S.ClDouble  -> D.HsType "Double"
	S.ClBoolean -> D.HsType "Boolean"
	S.ClString  -> D.HsType "String"
	S.ClVoid -> D.HsUnit

transformBody :: VarStates -> Maybe D.Name -> S.Body -> Maybe D.Expression
transformBody varStatesExternal mName (S.Body vars statements) = do
	let varStates = M.union (transformVars vars) varStatesExternal
	(varStates', modStatements) <- foldM transformStatement (varStates, []) statements
	modStatement <- case mName of
		Nothing -> return $ D.DoExecute returnUnitStatement
		Just name -> do
			VarState t i <- M.lookup name varStates'
			return $ D.DoExecute (beta [D.Access "return", D.Access (unvzName name i)])
	return $ D.DoExpression (reverse (modStatement:modStatements))

transformStatement :: (VarStates, [D.DoStatement]) -> S.Statement -> Maybe (VarStates, [D.DoStatement])
transformStatement (varStates, modStatements) = \case
	S.Execute (S.Name "readln") [S.LValue name] -> do
		VarState t i <- M.lookup (transformName name) varStates
		let j = succ i
		let varStates' = M.insert (transformName name) (VarState t j) varStates
		let modStatement = D.DoBind
			(unvzName (transformName name) j)
			(beta [D.Access "fmap", D.Access "read", D.Access "getLine"] `D.Typed` D.HsIO t)
		return (varStates', modStatement:modStatements)
	S.Execute name exprs -> do
		modExpr <- transformExecute
			varStates
			(transformName name)
			exprs
		let modStatement = D.DoExecute modExpr
		return (varStates, modStatement:modStatements)
	S.Assign name expr -> do
		VarState t i <- M.lookup
			(transformName name)
			varStates
		let j = succ i
		let varStates' = M.insert
			(transformName name)
			(VarState t j)
			varStates
		modExpr <- transformExpr id varStates expr
		let modStatement = D.DoLet (unvzName (transformName name) j) modExpr
		return (varStates', modStatement:modStatements)
	S.ForStatement (S.ForCycle [accName] iterName exprFrom exprTo body) -> do
		VarState t i <- M.lookup (transformName accName) varStates
		let j = succ i
		let varStates' = M.insert (transformName accName) (VarState t j) varStates
		modExprFrom <- transformExpr id varStates exprFrom
		modExprTo <- transformExpr id varStates exprTo
		let modRange = D.Range modExprFrom modExprTo
		let varStates''
			= M.insert (transformName accName)  (VarState t 0)
			$ M.insert (transformName iterName) (VarState (D.HsType "Integer") 0)
			$ varStates
		modBody <- transformBody
			varStates''
			(Just (transformName accName))
			body
		let modStatement = D.DoBind
			(unvzName (transformName accName) j)
			(beta
				[ D.Access "foldM"
				, D.Lambda
					[ transformName accName
					, transformName iterName
					]
					modBody
				, D.Access (unvzName (transformName accName) i)
				, modRange
				]
			)
		return (varStates', modStatement:modStatements)

transformExecute :: VarStates -> D.Name -> [S.Argument] -> Maybe D.Expression
transformExecute varStates = \case
	"writeln" -> \case
		[] -> return $ D.Beta (D.Access "putStrLn") (D.Quote "")
		exprs -> do
			modExprs <- forM exprs $ \case
				S.LValue name -> transformExpr id varStates (S.Access name)
				S.RValue expr -> transformExpr id varStates expr
			return
				$ D.Beta (D.Access "putStrLn")
				$ foldr1 (D.Binary "++")
				$ map (showNoString varStates)
				$ modExprs

transformFunc :: S.Func S.Body -> Maybe D.Def
transformFunc (S.Func S.NameMain params S.ClVoid clBody)
	 = do
		guard $ M.null params
		modBody <- transformBody M.empty Nothing clBody
		return $ D.ValueDef "main" [] modBody
transformFunc (S.Func name params clType clBody)
	 =  D.ValueDef (transformName name) paramNames
	<$> transformPureBody
		(hookName (transformName name))
		varStates
		(transformName name)
		clBody
	where
		paramNames
			= map transformName
			$ M.keys
			$ params
		varStates
			= M.union
				(uncurry M.singleton retVarState)
				(transformVars params)
		retVarState = initVarState name clType

transformPureBody :: (D.Name -> D.Name) -> VarStates -> D.Name -> S.Body -> Maybe D.Expression
transformPureBody funcHookName varStatesExternal name (S.Body vars statements) = do
	let varStates = M.union (transformVars vars) varStatesExternal
	(varStates', valueDefs) <- foldM (transformPureStatement funcHookName) (varStates, []) statements
	retValue <- do
		VarState t i <- M.lookup name varStates'
		return $ D.Access (unvzName (funcHookName name) i)
	return $ D.PureLet valueDefs retValue

transformPureStatement :: (D.Name -> D.Name) -> (VarStates, [D.ValueDef]) -> S.Statement -> Maybe (VarStates, [D.ValueDef])
transformPureStatement funcHookName (varStates, valueDefs) = \case
	S.Assign name expr -> do
		VarState t i <- M.lookup (transformName name) varStates
		let j = succ i
		let varStates' = M.insert (transformName name) (VarState t j) varStates
		modExpr <- transformExpr funcHookName varStates expr
		let valueDef = D.ValueDef (unvzName (funcHookName (transformName name)) j) [] modExpr
		return (varStates', valueDef:valueDefs)
	S.ForStatement (S.ForCycle [accName] iterName exprFrom exprTo body) -> do
		VarState t i <- M.lookup (transformName accName) varStates
		let j = succ i
		let varStates' = M.insert
			(transformName accName)
			(VarState t j)
			varStates
		modExprFrom <- transformExpr funcHookName varStates exprFrom
		modExprTo <- transformExpr funcHookName varStates exprTo
		let modRange = D.Range modExprFrom modExprTo
		let varStates''
			= M.insert (transformName accName)  (VarState t 0)
			$ M.insert (transformName iterName) (VarState (D.HsType "Integer") 0)
			$ varStates
		modBody <- transformPureBody
			funcHookName
			varStates''
			(transformName accName)
			body
		let valueDef = D.ValueDef
			(unvzName (funcHookName (transformName accName)) j) []
			(beta
				[ D.Access "foldl"
				, D.Lambda
					[ funcHookName (transformName accName)
					, transformName iterName
					]
					modBody
				, D.Access (unvzName (funcHookName (transformName accName)) i)
				, modRange
				]
			)
		return (varStates', valueDef:valueDefs)

beta = foldl1 D.Beta

unvzName name i = name ++ genericReplicate i '\''
unvzTable = M.fromList . map f . M.toList where
	f (name, VarState t i) = (unvzName name i, t)

showNoString :: VarStates -> D.Expression -> D.Expression
showNoString varStates = \case
	D.Access name -> case M.lookup name (unvzTable varStates) of
		Nothing -> error "showNoString: undefined"
		Just t -> case t of
			D.HsType "String" -> D.Access name
			_ -> D.Beta (D.Access "show") (D.Access name)
	a -> a -- TODO: bypass any D.Expression to transform underlying D.Access

transformExpr :: (D.Name -> D.Name) -> VarStates -> S.Expression -> Maybe D.Expression
transformExpr funcHookName varStates = \case
	S.Quote cs -> return $ D.Quote cs
	S.Number cs -> return $ D.Number cs
	S.Access name -> do
		VarState t i <- M.lookup (transformName name) varStates
		return $ D.Access (unvzName (funcHookName (transformName name)) i)
	S.Call name exprs -> do
		modExprs <- mapM (transformExpr funcHookName varStates) exprs
		return $ beta (D.Access (transformName name) : modExprs)
	S.Binary op x y -> do
		modX <- transformExpr funcHookName varStates x
		modY <- transformExpr funcHookName varStates y
		modOp <- case op of
			S.OpAdd -> return "+"
			S.OpSubtract -> return "-"
			S.OpMultiply -> return "*"
			S.OpDivide -> return "/"
		return $ D.Binary modOp modX modY

returnUnitStatement :: D.Expression
returnUnitStatement = D.Beta (D.Access "return") (D.Tuple [])
