{-# LANGUAGE FlexibleInstances #-}
module Sodium.Haskell.Convert (convert) where

import Data.List (genericReplicate)
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Sodium.Chloride.Program as S
import qualified  Sodium.Haskell.Program as D

convert :: S.VecProgram -> D.Program
convert = maybe (error "Sodium.Haskell.Convert") id . conv

class Conv s d | s -> d where
	conv :: s -> Maybe d

instance Conv S.VecProgram D.Program where
	conv (S.VecProgram funcs) = do
		funcDefs <- mapM conv funcs
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
		, "readLn"
		, "getLine"
		, "return"
		, "foldl"
		, "map"
		, "filter"
		, "undefined"
		, "main"
		, "import"
		, "_"
		]

data Name = Name S.Name S.Index
	deriving (Eq)

instance Conv Name D.Name where
	conv (Name name i) = case i of
		S.Index n -> return
			$ transformName name ++ genericReplicate n '\''
		S.Immutable -> return $ "const'" ++ transformName name
		S.Uninitialized -> return "undefined"

instance Conv S.ClType D.HsType where
	conv = return . \case
		S.ClInteger -> D.HsType "Int"
		S.ClDouble  -> D.HsType "Double"
		S.ClBoolean -> D.HsType "Bool"
		S.ClString  -> D.HsType "String"
		S.ClVoid -> D.HsUnit

newtype Pure a = Pure a

instance Conv S.VecBody D.Expression where
	conv (S.VecBody _ statements resultExprs) = do
		hsStatements <- mapM conv statements
		hsRetValues <- mapM conv resultExprs
		let hsStatement
			= D.DoExecute
			$ D.Beta (D.Access "return")
			$ D.Tuple hsRetValues
		return $ D.DoExpression (hsStatements ++ [hsStatement])

instance Conv S.VecForCycle D.Expression where
	conv (S.VecForCycle argIndices argExprs name exprRange clBody) = do
		hsRange <- conv exprRange
		hsArgExpr <- D.Tuple <$> mapM conv argExprs
		hsFoldLambda <- conv (FoldLambda argIndices name) <*> conv clBody
		return $ beta
			[ D.Access "foldM"
			, hsFoldLambda
			, hsArgExpr
			, hsRange
			]

instance Conv S.VecMultiIfBranch D.Expression where
	conv (S.VecMultiIfBranch leafs bodyElse) = do
		let convLeaf (expr, body)
			 =  D.IfExpression
			<$> conv expr
			<*> conv body
		leafGens <- mapM convLeaf leafs
		hsBodyElse <- conv bodyElse
		return $ foldr ($) hsBodyElse leafGens

--TODO: Bind/Let inference
instance Conv (S.IndicesList, S.VecStatement) D.DoStatement where
	conv (retIndices, S.VecExecute (S.ExecuteRead t) _)
		 =  D.DoBind
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> if t == S.ClString
			then return $ D.Access "getLine"
			else do
				hsType <- conv t
				return $ D.Access "readLn" `D.Typed` D.HsIO hsType
	conv (retIndices, S.VecExecute S.ExecuteWrite args)
		| null retIndices
		= case args of
			[S.VecRValue (S.VecCall (S.CallOperator S.OpShow) [arg])]
				-> D.DoExecute . D.Beta (D.Access "print") <$> conv arg
			args -> (<$> mapM conv args) $ \case
				[] -> D.DoExecute $ D.Beta (D.Access "putStrLn") (D.Quote "")
				hsExprs
					-> D.DoExecute
					 $ D.Beta (D.Access "putStrLn")
					 $ foldl1 (D.Binary "++")
					 $ hsExprs
	conv (retIndices, S.VecAssign expr)
		 =  D.DoLet
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> conv expr
	conv (retIndices, S.VecForStatement vecForCycle)
		 =  D.DoBind
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> conv vecForCycle
	conv (retIndices, S.VecMultiIfStatement vecMultiIfBranch)
		 =  D.DoBind
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> conv vecMultiIfBranch

instance Conv S.VecFunc D.ValueDef where
	conv (S.VecFunc (S.FuncSig S.NameMain params S.ClVoid) clBody) = do
		guard $ M.null params
		hsBody <- conv clBody
		return $ D.ValueDef (D.PatFunc "main" []) hsBody
	conv (S.VecFunc (S.FuncSig name params _) clBody)
		 =  D.ValueDef (D.PatFunc (transformName name) paramNames)
		<$> conv (Pure clBody)
		where paramNames = map transformName (M.keys params)

data FoldLambda = FoldLambda S.IndicesList S.Name

instance Conv FoldLambda (D.Expression -> D.Expression) where
	conv (FoldLambda indices name) = do
		hsNames <- conv (IndicesList indices)
		hsName <- conv (Name name S.Immutable)
		return $ D.Lambda [D.PatTuple hsNames, D.PatTuple [hsName]]

instance Conv (Pure S.VecBody) D.Expression where
	conv (Pure (S.VecBody _ statements resultExprs)) = msum
		[ do
			name1 <- case resultExprs of
				[S.VecAccess name i] -> return $ Name name i
				_ -> mzero
			let appToLast f xs = case reverse xs of
				(x:xs') -> (, reverse xs') <$> f x
				_ -> mzero
			let extractIndex = \case
				[(name, i)] -> return (Name name i)
				_ -> mzero
			let convStatement (indices, statement)
				 =  (,)
				<$> extractIndex indices
				<*> case statement of
					S.VecAssign expr -> conv expr
					S.VecForStatement vecForCycle -> conv (Pure vecForCycle)
					S.VecMultiIfStatement vecMultiIfBranch -> conv (Pure vecMultiIfBranch)
					S.VecBodyStatement vecBody -> conv (Pure vecBody)
					_ -> mzero
			((name2, hsExpr), statements) <- appToLast convStatement statements
			guard $ name1 == name2
			hsValueDefs <- mapM conv (map Pure statements)
			return $ pureLet hsValueDefs hsExpr
		, do
			hsValueDefs <- mapM conv (map Pure statements)
			hsRetValues <- mapM conv resultExprs
			return $ pureLet hsValueDefs (D.Tuple hsRetValues)
		]

instance Conv (Pure S.VecMultiIfBranch) D.Expression where
	conv (Pure (S.VecMultiIfBranch leafs bodyElse)) = do
		let convLeaf (expr, body)
			 =  D.IfExpression
			<$> conv expr
			<*> conv (Pure body)
		leafGens <- mapM convLeaf leafs
		hsBodyElse <- conv (Pure bodyElse)
		return $ foldr ($) hsBodyElse leafGens

instance Conv (Pure S.VecForCycle) D.Expression where
	conv (Pure (S.VecForCycle argIndices argExprs name exprRange clBody)) = do
		hsRange <- conv exprRange
		hsArgExpr <- D.Tuple <$> mapM conv argExprs
		hsFoldLambda
			<-  conv (FoldLambda argIndices name)
			<*> conv (Pure clBody)
		return $ beta [D.Access "foldl", hsFoldLambda, hsArgExpr, hsRange]


instance Conv (Pure (S.IndicesList, S.VecStatement)) D.ValueDef where
	conv (Pure (retIndices, statement))
		 =  D.ValueDef
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> case statement of
			S.VecAssign expr -> conv expr
			S.VecForStatement vecForCycle -> conv (Pure vecForCycle)
			S.VecMultiIfStatement vecMultiIfBranch -> conv (Pure vecMultiIfBranch)
			S.VecBodyStatement vecBody -> conv (Pure vecBody)
			_ -> mzero

beta = foldl1 D.Beta

pureLet [] = id
pureLet defs = D.PureLet defs

newtype IndicesList = IndicesList S.IndicesList

instance Conv IndicesList [D.Name] where
	conv (IndicesList xs) = mapM (conv . uncurry Name) xs

instance Conv S.VecArgument D.Expression where
	conv = \case
		S.VecLValue name i -> D.Access <$> conv (Name name i)
		S.VecRValue expr -> conv expr

instance Conv S.VecExpression D.Expression where
	conv (S.VecPrimary prim) = return $ case prim of
		S.Quote  cs -> D.Quote cs
		S.INumber intSection -> D.INumber intSection
		S.FNumber intSection fracSection
			-> D.FNumber intSection fracSection
		S.ENumber intSection fracSection eSign eSection
			-> D.ENumber intSection fracSection eSign eSection
		S.BTrue  -> D.BTrue
		S.BFalse -> D.BFalse
		S.Void   -> D.Tuple []
	conv (S.VecAccess name i) = D.Access <$> conv (Name name i)
	conv (S.VecCall callName exprs) = do
		hsExprs <- mapM conv exprs
		let binary hsOp = case hsExprs of
			hsExpr1:hsExpr2:hsExprs ->
				return $ beta (hsOp hsExpr1 hsExpr2 : hsExprs)
			_ -> mzero
		let unary hsOp = return $ beta (hsOp : hsExprs)
		case callName of
			S.CallOperator S.OpRange -> binary D.Range
			_ -> case convCallName callName of
				Left unop -> unary (D.Access unop)
				Right binop -> binary (D.Binary binop)
	conv (S.VecFold callName exprs range) = do
		hsArgExpr <- D.Tuple <$> mapM conv exprs
		hsRange <- conv range
		hsOp <- D.Access <$> either (const mzero) return (convCallName callName)
		return $ beta [D.Access "foldl", hsOp, hsArgExpr, hsRange]

convCallName = \case
	S.CallName name -> Left (transformName name)
	S.CallOperator op -> case op of
		S.OpNegate -> Left "negate"
		S.OpShow -> Left "show"
		S.OpProduct -> Left "product"
		S.OpSum -> Left "sum"
		S.OpAnd' -> Left "and"
		S.OpOr' -> Left "or"
		S.OpAdd -> Right "+"
		S.OpSubtract -> Right "-"
		S.OpMultiply -> Right "*"
		S.OpDivide -> Right "/"
		S.OpMore -> Right ">"
		S.OpLess -> Right "<"
		S.OpEquals -> Right "=="
		S.OpAnd -> Right "&&"
		S.OpOr -> Right "||"
		S.OpElem -> Right "elem"
		S.OpRange -> Right "enumFromTo"
