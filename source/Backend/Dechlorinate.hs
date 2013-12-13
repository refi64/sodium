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
import Success

dechlorinate :: S.VecProgram -> (Fail String) D.Program
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

dechlorinateBody :: S.Vars -> S.VecBody -> (Fail String) D.Expression
dechlorinateBody externalVars (S.VecBody vars statements resultExprs) = do
	let vars' = M.union vars externalVars
	hsStatements <- mapM (dechlorinateStatement vars') statements
	hsRetValues <- mapM dechlorinateExpression resultExprs
	let hsStatement
		= D.DoExecute
		$ D.Beta (D.Access "return")
		$ D.Tuple hsRetValues
	return $ D.DoExpression (hsStatements ++ [hsStatement])

dechlorinateStatement :: S.Vars -> S.VecStatement -> (Fail String) D.DoStatement
dechlorinateStatement vars = \case
	S.VecExecute retIndices S.ExecuteRead [S.VecLValue name i] -> do
		t <- annotate (M.lookup name vars) 0 ("Could not access " ++ show name)
		let hsRetPat
			= D.PatTuple
			$ map
				(uncurry dechlorinateName)
				(M.toList retIndices)
		let hsExpr = beta [D.Access "fmap", D.Access "read", D.Access "getLine"] `D.Typed` D.HsIO (dechlorinateType t)
		return $ D.DoBind hsRetPat hsExpr
	S.VecExecute retIndices S.ExecuteWrite args -> do
		-- WriteLn can't change its arguments
		guard $ M.null retIndices
		hsArgs <- mapM dechlorinateArgument args
		return $ case hsArgs of
			[] -> D.DoExecute $ D.Beta (D.Access "putStrLn") (D.Quote "")
			hsExprs
				-> D.DoExecute
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
		hsBody <- dechlorinateBody vars clBody
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

dechlorinateFunc :: S.Func S.VecBody -> (Fail String) D.Def
dechlorinateFunc (S.Func S.NameMain params S.ClVoid clBody)
	= do
		guard $ M.null params
		hsBody <- dechlorinateBody M.empty clBody
		return $ D.ValueDef (D.PatFunc "main" []) hsBody
dechlorinateFunc (S.Func name params retType clBody)
	 =  D.ValueDef (D.PatFunc (transformName name) paramNames)
	<$> dechlorinatePureBody params clBody
	where
		paramNames
			= map transformName
			$ M.keys params

dechlorinatePureBody :: S.Vars -> S.VecBody -> (Fail String) D.Expression
dechlorinatePureBody externalVars (S.VecBody vars statements resultExprs) = do
	let vars' = M.union vars externalVars
	hsValueDefs <- mapM (dechlorinatePureStatement vars') statements
	hsRetValues <- mapM dechlorinateExpression resultExprs
	return $ D.PureLet hsValueDefs (D.Tuple hsRetValues)

dechlorinatePureStatement :: S.Vars -> S.VecStatement -> (Fail String) D.ValueDef
dechlorinatePureStatement vars = \case
	S.VecAssign name i expr -> do
		hsExpr <- dechlorinateExpression expr
		return $ D.ValueDef (D.PatFunc (dechlorinateName name i) []) hsExpr
	S.VecForStatement retIndices (S.VecForCycle argIndices name exprFrom exprTo clBody) -> do
		hsRange
			<-  D.Range
			<$> dechlorinateExpression exprFrom
			<*> dechlorinateExpression exprTo
		hsBody <- dechlorinatePureBody vars clBody
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
						$ M.keys argIndices
					, D.PatTuple [transformName name]
					]
					hsBody
				, hsArgExpr
				, hsRange
				]
			)
	st -> error (show st)

beta = foldl1 D.Beta

dechlorinateArgument :: S.VecArgument -> (Fail String) D.Expression
dechlorinateArgument = \case
	S.VecLValue name i -> return $ D.Access (dechlorinateName name i)
	S.VecRValue expr -> dechlorinateExpression expr

dechlorinateExpression :: S.VecExpression -> (Fail String) D.Expression
dechlorinateExpression = \case
	S.VecPrimary prim -> return $ case prim of
		S.Quote  cs -> D.Quote cs
		S.Number cs -> D.Number cs
		S.BTrue  -> D.BTrue
		S.BFalse -> D.BFalse
		S.Void   -> D.Tuple []
	S.VecAccess name i -> return $ D.Access (dechlorinateName name i)
	S.VecCall callName exprs -> do
		hsExprs <- mapM dechlorinateExpression exprs
		let binary hsOp = case hsExprs of
			hsExpr1:hsExpr2:hsExprs -> 
				return $ beta (D.Binary hsOp hsExpr1 hsExpr2 : hsExprs)
			e -> error (show e)
		case callName of
			S.CallName name
				-> return
				 $ beta (D.Access (transformName name) : hsExprs)
			S.CallOperator op -> case op of
				S.OpShow -> do
					case hsExprs of
						[hsExpr1] -> return (D.Access "show" `D.Beta` hsExpr1)
						e -> error (show e)
				S.OpAdd -> binary "+"
				S.OpSubtract -> binary "-"
				S.OpMultiply -> binary "*"
				S.OpDivide -> binary "/"
				S.OpMore -> binary ">"
				S.OpLess -> binary "<"
				S.OpEquals -> binary "=="
				S.OpAnd -> binary "&&"
				S.OpOr -> binary "||"
