{-# LANGUAGE LambdaCase #-}

module Backend.Transform
	( transform
	) where

import Data.List (genericReplicate)
import Control.Monad (foldM)
import Control.Applicative ((<$>))
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Frontend.Program as S
import qualified  Backend.Program as D

transform :: S.Program -> Maybe D.Program
transform (S.Program funcs vars body) = do
	modBody <- transformBody (transformVars vars) Nothing body
	let mainDef = D.ValueDef "main" [] modBody
	funcDefs <- mapM transformFunc funcs
	return $ D.Program (mainDef:funcDefs) ["Control.Monad"]

data VarState = VarState D.HsType Integer
type VarStates = M.Map D.Name VarState

initVarState name pasType = (transformName name, VarState (transformType pasType) (-1))

transformVars :: S.Vars -> VarStates
transformVars (S.Vars vardecls)
	= M.fromList
	$ flip map vardecls
	$ \(S.VarDecl name pasType) ->
		initVarState name pasType

-- WARNING: Name handling is broken. Beware, because
-- strange errors may appear in generated code.
-- TODO: Name management system
transformName :: S.Name -> D.Name
transformName = id

hookName :: D.Name -> D.Name -> D.Name
hookName u v = if u == v then v ++ "_" else v

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
	S.Execute "readln" [S.Access name] -> do
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
		modExpr <- transformExpr id varStates expr
		let modStatement = D.DoLet (unvzName name j) modExpr
		return (varStates', modStatement:modStatements)
	S.ForCycle [accName] iterName exprFrom exprTo body -> do
		VarState t i <- M.lookup accName varStates
		let j = succ i
		let varStates' = M.insert accName (VarState t j) varStates
		modExprFrom <- transformExpr id varStates exprFrom
		modExprTo <- transformExpr id varStates exprTo
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
			modExprs <- mapM (transformExpr id varStates) exprs
			Just
				$ D.Beta (D.Access "putStrLn")
				$ foldr1 (D.Binary "++")
				$ map (showNoString varStates)
				$ modExprs

transformFunc :: S.Func -> Maybe D.Def
transformFunc (S.Func name params pasType vars body)
	 =  D.ValueDef name paramNames
	<$> transformPureBody (hookName name) varStates name body
	where
		paramNames
			= map (\(S.VarDecl name _) -> name)
			$ (\(S.Vars varDecls) -> varDecls)
			$ params
		varStates
			= foldr M.union (uncurry M.singleton retVarState)
			$ map transformVars
			[ vars
			, params
			]
		retVarState = initVarState name pasType


transformPureBody :: (D.Name -> D.Name) -> VarStates -> D.Name -> S.Body -> Maybe D.Expression
transformPureBody funcHookName varStates name (S.Body statements) = do
	(varStates', valueDefs) <- foldM (transformPureStatement funcHookName) (varStates, []) statements
	retValue <- do
		VarState t i <- M.lookup name varStates'
		return $ D.Access (unvzName (funcHookName name) i)
	return $ D.PureLet valueDefs retValue

transformPureStatement :: (D.Name -> D.Name) -> (VarStates, [D.ValueDef]) -> S.Statement -> Maybe (VarStates, [D.ValueDef])
transformPureStatement funcHookName (varStates, valueDefs) = \case
	S.Assign name expr -> do
		VarState t i <- M.lookup name varStates
		let j = succ i
		let varStates' = M.insert name (VarState t j) varStates
		modExpr <- transformExpr funcHookName varStates expr
		let valueDef = D.ValueDef (unvzName (funcHookName name) j) [] modExpr
		return (varStates', valueDef:valueDefs)
	S.ForCycle [accName] iterName exprFrom exprTo body -> do
		VarState t i <- M.lookup accName varStates
		let j = succ i
		let varStates' = M.insert accName (VarState t j) varStates
		modExprFrom <- transformExpr funcHookName varStates exprFrom
		modExprTo <- transformExpr funcHookName varStates exprTo
		let modRange = D.Range modExprFrom modExprTo
		let varStates''
			= M.insert accName (VarState t 0)
			$ M.insert iterName (VarState D.HsInteger 0)
			$ varStates
		modBody <- transformPureBody funcHookName varStates'' accName body
		let valueDef = D.ValueDef
			(unvzName (funcHookName accName) j) []
			(beta [D.Access "foldl", D.Lambda [funcHookName accName, iterName] modBody, D.Access (unvzName (funcHookName accName) i), modRange])
		return (varStates', valueDef:valueDefs)

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

transformExpr :: (D.Name -> D.Name) -> VarStates -> S.Expression -> Maybe D.Expression
transformExpr funcHookName varStates = \case
	S.Quote cs -> return $ D.Quote cs
	S.Number cs -> return $ D.Number cs
	S.Access name -> do
		VarState t i <- M.lookup name varStates
		return $ D.Access (unvzName (funcHookName name) i)
	S.Call name exprs -> do
		modExprs <- mapM (transformExpr funcHookName varStates) exprs
		return $ beta (D.Access name : modExprs)
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
