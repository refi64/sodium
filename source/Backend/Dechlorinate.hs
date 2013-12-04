{-# LANGUAGE LambdaCase #-}

module Backend.Dechlorinate
	( dechlorinate
	) where

import Data.List (genericReplicate)
import Control.Monad (forM, foldM, guard)
import Control.Applicative
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Chloride.Chloride as S
import qualified  Backend.Program  as D

dechlorinate :: S.VecProgram -> Maybe D.Program
dechlorinate (S.VecProgram funcs) = do
	funcDefs <- mapM dechlorinateFunc funcs
	return $ D.Program funcDefs ["Control.Monad"]

data VarState = VarState D.HsType Integer
type VarStates = M.Map D.Name VarState

-- WARNING: Name handling is broken. Beware, because
-- strange errors may appear in generated code.
-- TODO: Name management system
transformName :: S.Name -> D.Name
transformName = \case
	S.NameMain -> "main"
	S.Name cs  -> cs
	S.NameUnique name -> transformName name ++ "_"

dechlorinateName :: S.Name -> Integer -> D.Name
dechlorinateName name i
	| i > 0 = transformName name ++ genericReplicate (pred i) '\''
	| otherwise = error $ "Accessing uninitialized variable " ++ show name

dechlorinateType :: S.ClType -> D.HsType
dechlorinateType = \case
	S.ClInteger -> D.HsType "Integer"
	S.ClDouble  -> D.HsType "Double"
	S.ClBoolean -> D.HsType "Boolean"
	S.ClString  -> D.HsType "String"
	S.ClVoid -> D.HsUnit

dechlorinateBody :: S.Vars -> [S.Name] -> S.VecBody -> Maybe D.Expression
dechlorinateBody externalVars rets (S.VecBody vars statements indices) = do
	let vars' = M.union vars externalVars
	hsStatements <- mapM (dechlorinateStatement vars') statements
	let hsStatement
		= D.DoExecute
		$ D.Beta (D.Access "return")
		$ D.Tuple
		$ map (D.Access . uncurry dechlorinateName)
		$ M.toList
		$ M.filterWithKey (\name _ -> name `elem` rets)
		$ indices
	return $ D.DoExpression (hsStatements ++ [hsStatement])

dechlorinateStatement :: S.Vars -> S.VecStatement -> Maybe D.DoStatement
dechlorinateStatement vars = \case
	S.VecExecute (S.Name "readln") [S.VecLValue name i] -> do
		t <- M.lookup name vars
		return $ D.DoBind
			-- (succ i) is a DIRTY HACK!!! Remove it
			-- as soon as proper typechecking is
			-- implemented
			(D.PatTuple [dechlorinateName name (succ i)])
			(beta [D.Access "fmap", D.Access "read", D.Access "getLine"] `D.Typed` D.HsIO (dechlorinateType t))
	S.VecExecute (S.Name "writeln") args -> case args of
		[] -> return $ D.DoExecute $ D.Beta (D.Access "putStrLn") (D.Quote "")
		args' -> do
			hsExprs <- forM args' $ \case
				S.VecLValue name i -> do
					t <- M.lookup name vars
					let mShow
						| t == S.ClString = id
						| otherwise = D.Beta (D.Access "show")
					mShow <$> dechlorinateExpression (S.VecAccess name i)
				S.VecRValue expr -> dechlorinateExpression expr
			return
				$ D.DoExecute
				$ D.Beta (D.Access "putStrLn")
				$ foldr1 (D.Binary "++")
				$ hsExprs
	S.VecAssign name i expr -> do
		hsExpr <- dechlorinateExpression expr
		return $ D.DoLet (dechlorinateName name i) hsExpr
	S.VecForStatement retIndices (S.VecForCycle argIndices name exprFrom exprTo clBody) -> do
		hsRange
			<-  D.Range
			<$> dechlorinateExpression exprFrom
			<*> dechlorinateExpression exprTo
		hsBody <- dechlorinateBody vars (M.keys argIndices) clBody
		let hsArgExpr
			= D.Tuple
			$ map D.Access
			$ map
				(uncurry dechlorinateName)
				(M.toList argIndices)
		let hsRetPat
			= D.PatTuple
			$ map
				(uncurry dechlorinateName)
				(M.toList retIndices)
		return $ D.DoBind
			hsRetPat
			(beta
				[ D.Access "foldM"
				, D.Lambda
					[ D.PatTuple
						$ map transformName
						$ M.keys retIndices
					, D.PatTuple [transformName name]
					]
					hsBody
				, hsArgExpr
				, hsRange
				]
			)
	st -> error (show st)

dechlorinateFunc :: S.Func S.VecBody -> Maybe D.Def
dechlorinateFunc (S.Func S.NameMain params S.ClVoid S.NameMain clBody)
	= do
		guard $ M.null params
		hsBody <- dechlorinateBody M.empty [] clBody
		return $ D.ValueDef (D.PatFunc "main" []) hsBody
dechlorinateFunc (S.Func name params retType retName clBody)
	 =  D.ValueDef (D.PatFunc (transformName name) paramNames)
	<$> dechlorinatePureBody vars [retName] clBody
	where
		paramNames
			= map transformName
			$ M.keys
			$ params
		vars = M.union retVars params
		retVars = M.singleton retName retType

dechlorinatePureBody :: S.Vars -> [S.Name] -> S.VecBody -> Maybe D.Expression
dechlorinatePureBody externalVars rets (S.VecBody vars statements indices) = do
	let vars' = M.union vars externalVars
	hsValueDefs <- mapM (dechlorinatePureStatement vars') statements
	let hsRetValue
		= D.Tuple
		$ map (D.Access . uncurry dechlorinateName)
		$ M.toList
		$ M.filterWithKey (\name _ -> name `elem` rets)
		$ indices
	return $ D.PureLet hsValueDefs hsRetValue

dechlorinatePureStatement :: S.Vars -> S.VecStatement -> Maybe D.ValueDef
dechlorinatePureStatement vars = \case
	S.VecAssign name i expr -> do
		hsExpr <- dechlorinateExpression expr
		return $ D.ValueDef (D.PatFunc (dechlorinateName name i) []) hsExpr
	S.VecForStatement retIndices (S.VecForCycle argIndices name exprFrom exprTo clBody) -> do
		hsRange
			<-  D.Range
			<$> dechlorinateExpression exprFrom
			<*> dechlorinateExpression exprTo
		hsBody <- dechlorinatePureBody vars (M.keys argIndices) clBody
		let hsArgExpr
			= D.Tuple
			$ map D.Access
			$ map
				(uncurry dechlorinateName)
				(M.toList argIndices)
		let hsRetPat
			= D.PatTuple
			$ map
				(uncurry dechlorinateName)
				(M.toList retIndices)
		return $ D.ValueDef
			hsRetPat
			(beta
				[ D.Access "foldl"
				, D.Lambda
					[ D.PatTuple
						$ map transformName
						$ M.keys retIndices
					, D.PatTuple [transformName name]
					]
					hsBody
				, hsArgExpr
				, hsRange
				]
			)
	st -> error (show st)

beta = foldl1 D.Beta

dechlorinateExpression :: S.VecExpression -> Maybe D.Expression
dechlorinateExpression = \case
	S.VecPrimary (S.Quote  cs) -> return $ D.Quote cs
	S.VecPrimary (S.Number cs) -> return $ D.Number cs
	S.VecAccess name i -> return $ D.Access (dechlorinateName name i)
	S.VecCall name exprs -> do
		hsExprs <- mapM dechlorinateExpression exprs
		return $ beta (D.Access (transformName name) : hsExprs)
	S.VecBinary op expr1 expr2 -> do
		hsExpr1 <- dechlorinateExpression expr1
		hsExpr2 <- dechlorinateExpression expr2
		hsOp <- case op of
			S.OpAdd -> return "+"
			S.OpSubtract -> return "-"
			S.OpMultiply -> return "*"
			S.OpDivide -> return "/"
		return $ D.Binary hsOp hsExpr1 hsExpr2
