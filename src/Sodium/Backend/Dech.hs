{-# LANGUAGE FlexibleInstances #-}

module Sodium.Backend.Dech (Dech(..)) where

import Data.List (genericReplicate)
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Sodium.Chloride.Program as S
import qualified  Sodium.Backend.Program as D
import Sodium.Success

class Dech s d | s -> d where
	dech :: s -> (Fail String) d

instance Dech S.VecProgram D.Program where
	dech (S.VecProgram funcs) = do
		funcDefs <- mapM dech funcs
		return $ D.Program (map D.Def funcDefs)
			[ "Control.Monad"
			, "Control.Applicative"
			]
			[ "LambdaCase"
			, "TupleSections"
			]

transformName :: S.Name -> D.Name
transformName = \case
	S.NameMain -> "main"
	S.Name cs
		-> (if reserved cs
			then ("_'"++)
			else id) cs
	S.NameUnique name -> transformName name ++ "'_"
	where reserved = flip elem
		[ "let"
		, "show"
		, "read"
		, "return"
		, "foldl"
		, "map"
		, "filter"
		, "undefined"
		]

data Name = Name S.Name Integer
	deriving (Eq)

instance Dech Name D.Name where
	dech (Name name i)
		| i < 0
			= return
			$ "const'"
				++ genericReplicate (pred (-i)) '\''
				++ transformName name
		| i > 0
			= return
			$ transformName name
				++ genericReplicate (pred i) '\''
		| otherwise
			= return "undefined"

instance Dech S.ClType D.HsType where
	dech = return . \case
		S.ClInteger -> D.HsType "Int"
		S.ClDouble  -> D.HsType "Double"
		S.ClBoolean -> D.HsType "Bool"
		S.ClString  -> D.HsType "String"
		S.ClVoid -> D.HsUnit

newtype Pure a = Pure a

instance Dech S.VecBody D.Expression where
	dech (S.VecBody _ statements resultExprs) = do
		hsStatements <- mapM dech statements
		hsRetValues <- mapM dech resultExprs
		let hsStatement
			= D.DoExecute
			$ D.Beta (D.Access "return")
			$ D.Tuple hsRetValues
		return $ D.DoExpression (hsStatements ++ [hsStatement])

instance Dech S.VecForCycle D.Expression where
	dech (S.VecForCycle argIndices argExprs name exprFrom exprTo clBody) = do
		hsRange <- dech $ Range exprFrom exprTo
		hsArgExpr <- D.Tuple <$> mapM dech argExprs
		hsFoldLambda <- dech (FoldLambda argIndices name) <*> dech clBody
		return $ beta
			[ D.Access "foldM"
			, hsFoldLambda
			, hsArgExpr
			, hsRange
			]

instance Dech S.VecMultiIfBranch D.Expression where
	dech (S.VecMultiIfBranch leafs bodyElse) = do
		let dechLeaf (expr, body)
			 =  D.IfExpression
			<$> dech expr
			<*> dech body
		leafGens <- mapM dechLeaf leafs
		hsBodyElse <- dech bodyElse
		return $ foldr ($) hsBodyElse leafGens

instance Dech S.VecStatement D.DoStatement where
	dech = \case
		S.VecExecute retIndices (S.ExecuteRead t) _ -> do
			hsRetPat <- D.PatTuple <$> dech (IndicesList retIndices)
			hsExpr <- if t == S.ClString
				then return $ D.Access "getLine"
				else do
					hsType <- dech t
					return
						$ D.Binary "<$>" (D.Access "read") (D.Access "getLine")
						`D.Typed` D.HsIO hsType 
			return $ D.DoBind hsRetPat hsExpr
		S.VecExecute retIndices S.ExecuteWrite args -> do
			-- WriteLn can't change its arguments
			guard $ null retIndices
			hsArgs <- mapM dech args
			return $ case hsArgs of
				[] -> D.DoExecute $ D.Beta (D.Access "putStrLn") (D.Quote "")
				hsExprs
					-> D.DoExecute
					 $ D.Beta (D.Access "putStrLn")
					 $ foldr1 (D.Binary "++")
					 $ hsExprs
		S.VecAssign name i expr -> do
			hsExpr <- dech expr
			D.DoLet <$> dech (Name name i) <*> return hsExpr
		S.VecForStatement retIndices vecForCycle
			-> wrap retIndices vecForCycle
		S.VecMultiIfStatement retIndices vecMultiIfBranch
			-> wrap retIndices vecMultiIfBranch
		_ -> mzero
		where wrap retIndices vecPart = do
			hsRetPat <- D.PatTuple <$> dech (IndicesList retIndices)
			D.DoBind hsRetPat <$> dech vecPart

instance Dech S.VecFunc D.ValueDef where
	dech (S.VecFunc (S.FuncSig S.NameMain params S.ClVoid) clBody) = do
		guard $ M.null params
		hsBody <- dech clBody
		return $ D.ValueDef (D.PatFunc "main" []) hsBody
	dech (S.VecFunc (S.FuncSig name params _) clBody)
		 =  D.ValueDef (D.PatFunc (transformName name) paramNames)
		<$> dech (Pure clBody)
		where paramNames = map transformName (M.keys params)

data Range = Range S.VecExpression S.VecExpression

instance Dech Range D.Expression where
	dech (Range exprFrom exprTo)
		 =  D.Range
		<$> dech exprFrom
		<*> dech exprTo

data FoldLambda = FoldLambda S.IndicesList S.Name

instance Dech FoldLambda (D.Expression -> D.Expression) where
	dech (FoldLambda indices name) = do
		hsNames <- dech (IndicesList indices)
		hsName <- dech (Name name (-1))
		return $ D.Lambda [D.PatTuple hsNames, D.PatTuple [hsName]]

instance Dech (Pure S.VecBody) D.Expression where
	dech (Pure (S.VecBody _ statements resultExprs)) = msum
		[ do
			name1 <- case resultExprs of
				[S.VecAccess name i] -> return $ Name name i
				_ -> mzero
			let appToLast f xs = case reverse xs of
				(x:xs') -> (, reverse xs') <$> f x
				_ -> mzero
			let dechStatement = \case
				S.VecAssign name i expr
					-> (Name name i, ) <$> dech expr
				S.VecForStatement [(name, i)] vecForCycle
					-> (Name name i, ) <$> dech (Pure vecForCycle)
				S.VecMultiIfStatement [(name, i)] vecMultiIfBranch
					-> (Name name i, ) <$> dech (Pure vecMultiIfBranch)
				S.VecBodyStatement [(name, i)] vecBody
					-> (Name name i, ) <$> dech (Pure vecBody)
				_ -> mzero
			((name2, hsExpr), statements) <- appToLast dechStatement statements
			guard $ name1 == name2
			hsValueDefs <- mapM dech (map Pure statements)
			return $ pureLet hsValueDefs hsExpr
		, do
			hsValueDefs <- mapM dech (map Pure statements)
			hsRetValues <- mapM dech resultExprs
			return $ pureLet hsValueDefs (D.Tuple hsRetValues)
		]

instance Dech (Pure S.VecMultiIfBranch) D.Expression where
	dech (Pure (S.VecMultiIfBranch leafs bodyElse)) = do
		let dechLeaf (expr, body)
			 =  D.IfExpression
			<$> dech expr
			<*> dech (Pure body)
		leafGens <- mapM dechLeaf leafs
		hsBodyElse <- dech (Pure bodyElse)
		return $ foldr ($) hsBodyElse leafGens

instance Dech (Pure S.VecForCycle) D.Expression where
	dech (Pure (S.VecForCycle argIndices argExprs name exprFrom exprTo clBody)) = do
		hsRange <- dech $ Range exprFrom exprTo
		hsArgExpr <- D.Tuple <$> mapM dech argExprs
		hsFoldLambda
			<-  dech (FoldLambda argIndices name)
			<*> dech (Pure clBody)
		return $ beta [D.Access "foldl", hsFoldLambda, hsArgExpr, hsRange]

instance Dech (Pure S.VecStatement) D.ValueDef where
	dech (Pure statement) = case statement of
		S.VecAssign name i expr -> do
			hsExpr <- dech expr
			hsName <- dech (Name name i)
			return $ D.ValueDef (D.PatFunc hsName []) hsExpr
		S.VecForStatement retIndices vecForCycle
			-> wrap retIndices vecForCycle
		S.VecMultiIfStatement retIndices vecMultiIfBranch
			-> wrap retIndices vecMultiIfBranch
		S.VecBodyStatement retIndices vecBody
			-> wrap retIndices vecBody
		_ -> mzero
		where wrap retIndices vecPart
			 = D.ValueDef
			<$> (D.PatTuple <$> dech (IndicesList retIndices))
			<*> dech (Pure vecPart)

beta = foldl1 D.Beta

pureLet [] = id
pureLet defs = D.PureLet defs

newtype IndicesList = IndicesList S.IndicesList

instance Dech IndicesList [D.Name] where
	dech (IndicesList xs) = mapM (dech . uncurry Name) xs

instance Dech S.VecArgument D.Expression where
	dech = \case
		S.VecLValue name i -> D.Access <$> dech (Name name i)
		S.VecRValue expr -> dech expr

instance Dech S.VecExpression D.Expression where
	dech (S.VecPrimary prim) = return $ case prim of
		S.Quote  cs -> D.Quote cs
		S.INumber intSection -> D.INumber intSection
		S.FNumber intSection fracSection
			-> D.FNumber intSection fracSection
		S.ENumber intSection fracSection eSign eSection
			-> D.ENumber intSection fracSection eSign eSection
		S.BTrue  -> D.BTrue
		S.BFalse -> D.BFalse
		S.Void   -> D.Tuple []
	dech (S.VecAccess name i) = D.Access <$> dech (Name name i)
	dech (S.VecCall callName exprs) = do
		hsExprs <- mapM dech exprs
		let binary hsOp = case hsExprs of
			hsExpr1:hsExpr2:hsExprs ->
				return $ beta (hsOp hsExpr1 hsExpr2 : hsExprs)
			_ -> mzero
		let unary hsOp = case hsExprs of
			hsExpr1:hsExprs ->
				return $ beta (hsOp hsExpr1 : hsExprs)
			_ -> mzero
		case callName of
			S.CallName name
				-> return
				 $ beta (D.Access (transformName name) : hsExprs)
			S.CallOperator op -> case op of
				S.OpNegate -> unary (D.Beta $ D.Access "negate")
				S.OpShow -> unary (D.Beta $ D.Access "show")
				S.OpAdd -> binary (D.Binary "+")
				S.OpSubtract -> binary (D.Binary "-")
				S.OpMultiply -> binary (D.Binary "*")
				S.OpDivide -> binary (D.Binary "/")
				S.OpMore -> binary (D.Binary ">")
				S.OpLess -> binary (D.Binary "<")
				S.OpEquals -> binary (D.Binary "==")
				S.OpAnd -> binary (D.Binary "&&")
				S.OpOr -> binary (D.Binary "||")
				S.OpElem -> binary (D.Binary "`elem`")
				S.OpRange -> binary D.Range