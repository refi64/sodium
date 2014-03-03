{-# LANGUAGE LambdaCase, TupleSections #-}

module Backend.Dechlorinate
	( dechlorinate
	) where

import Data.List (genericReplicate)
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Chloride.Program as S
import qualified  Backend.Program as D
import Success

dechlorinate :: S.VecProgram -> (Fail String) D.Program
dechlorinate (S.VecProgram funcs) = do
	funcDefs <- mapM dechlorinateFunc funcs
	return $ D.Program funcDefs
		[ "Control.Monad"
		, "Control.Applicative"
		]
		[ "LambdaCase"
		, "TupleSections"
		]

data VarState = VarState D.HsType Integer
type VarStates = M.Map D.Name VarState

transformName :: S.Name -> D.Name
transformName = \case
	S.NameMain -> "main"
	S.Name cs
		-> (if reserved cs
			then ("_'"++)
			else id) cs
	S.NameUnique name -> transformName name ++ "'_"
	where
		reserved
			= flip elem
			[ "let"
			, "show"
			, "read"
			, "return"
			, "foldl"
			, "map"
			, "filter"
			, "undefined"
			]

dechlorinateName :: S.Name -> Integer -> (Fail String) D.Name
dechlorinateName name i
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

dechlorinateType :: S.ClType -> D.HsType
dechlorinateType = \case
	S.ClInteger -> D.HsType "Int"
	S.ClDouble  -> D.HsType "Double"
	S.ClBoolean -> D.HsType "Bool"
	S.ClString  -> D.HsType "String"
	S.ClVoid -> D.HsUnit

dechlorinateBody :: S.VecBody -> (Fail String) D.Expression
dechlorinateBody (S.VecBody _ statements resultExprs) = do
	hsStatements <- mapM dechlorinateStatement statements
	hsRetValues <- mapM dechlorinateExpression resultExprs
	let hsStatement
		= D.DoExecute
		$ D.Beta (D.Access "return")
		$ D.Tuple hsRetValues
	return $ D.DoExpression (hsStatements ++ [hsStatement])

dechlorinateStatement :: S.VecStatement -> (Fail String) D.DoStatement
dechlorinateStatement = \case
	S.VecExecute retIndices (S.ExecuteRead t) [S.VecLValue name i] -> do
		hsRetPat <- D.PatTuple <$> dechlorinateIndicesList retIndices
		let hsExpr
			| t == S.ClString = D.Access "getLine"
			| otherwise = D.Binary "<$>" (D.Access "read") (D.Access "getLine")
				`D.Typed` D.HsIO (dechlorinateType t)
		return $ D.DoBind hsRetPat hsExpr
	S.VecExecute retIndices S.ExecuteWrite args -> do
		-- WriteLn can't change its arguments
		guard $ null retIndices
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
		D.DoLet <$> dechlorinateName name i <*> return hsExpr
	S.VecForStatement retIndices (S.VecForCycle argIndices name exprFrom exprTo clBody) -> do
		hsRange <- dechlorinateRange exprFrom exprTo
		hsBody <- dechlorinateBody clBody
		hsArgExpr
			<-  D.Tuple
			<$> (map D.Access
			<$> dechlorinateIndicesList argIndices)
		hsRetPat
			<-  D.PatTuple
			<$> dechlorinateIndicesList retIndices
		hsFoldLambda
			<-  dechlorinateFoldLambda argIndices name
			<*> return hsBody
		return $ D.DoBind
			hsRetPat
			(beta
				[ D.Access "foldM"
				, hsFoldLambda
				, hsArgExpr
				, hsRange
				]
			)
	S.VecIfStatement retIndices (S.VecIfBranch expr bodyThen bodyElse) -> do
		hsExpr <- dechlorinateExpression expr
		hsBodyThen <- dechlorinateBody bodyThen
		hsBodyElse <- dechlorinateBody bodyElse
		hsRetPat
			<-  D.PatTuple
			<$> dechlorinateIndicesList retIndices
		return $ D.DoBind
			hsRetPat
			(D.IfExpression hsExpr hsBodyThen hsBodyElse)
	_ -> mzero

dechlorinateFunc :: S.VecFunc -> (Fail String) D.Def
dechlorinateFunc (S.VecFunc (S.FuncSig S.NameMain params S.ClVoid) clBody)
	= do
		guard $ M.null params
		hsBody <- dechlorinateBody clBody
		return $ D.ValueDef (D.PatFunc "main" []) hsBody
dechlorinateFunc (S.VecFunc (S.FuncSig name params retType) clBody)
	 =  D.ValueDef (D.PatFunc (transformName name) paramNames)
	<$> dechlorinatePureBody clBody
	where
		paramNames
			= map transformName
			$ M.keys params

dechlorinateRange :: S.VecExpression -> S.VecExpression -> (Fail String) D.Expression
dechlorinateRange exprFrom exprTo
	 =  D.Range
	<$> dechlorinateExpression exprFrom
	<*> dechlorinateExpression exprTo

dechlorinateFoldLambda :: S.IndicesList -> S.Name -> (Fail String) (D.Expression -> D.Expression)
dechlorinateFoldLambda indices name = do
	hsNames <- dechlorinateIndicesList indices
	hsName <- dechlorinateName name (-1)
	return $ D.Lambda
		[ D.PatTuple hsNames
		, D.PatTuple [hsName]
		]

dechlorinatePureBody
	:: S.VecBody
	-> (Fail String) D.Expression
dechlorinatePureBody (S.VecBody _ statements resultExprs) = msum
	[ do
		name1 <- case resultExprs of
			[S.VecAccess name i] -> return (name, i)
			_ -> mzero
		let appToLast f xs = case reverse xs of
			(x:xs') -> (, reverse xs') <$> f x
			_ -> mzero
		((name2, hsExpr), statements)
			<- flip appToLast statements
			$ \case
				S.VecAssign name i expr -> do
					hsExpr <- dechlorinateExpression expr
					return ((name, i), hsExpr)
				S.VecCaseStatement [(name, i)] vecCaseBranch -> do
					hsExpr <- dechlorinatePureVecCaseBranch vecCaseBranch
					return ((name, i), hsExpr)
				_ -> mzero
		guard $ name1 == name2
		hsValueDefs <- mapM dechlorinatePureStatement statements
		return $ pureLet hsValueDefs hsExpr
	, do
		hsValueDefs <- mapM dechlorinatePureStatement statements
		hsRetValues <- mapM dechlorinateExpression resultExprs
		return $ pureLet hsValueDefs (D.Tuple hsRetValues)
	]

dechlorinatePureVecCaseBranch (S.VecCaseBranch expr leafs bodyElse) = do
	(caseExpr, wrap) <- case expr of
		S.VecAccess name i -> do
			hsName <- dechlorinateName name i
			return (D.Access hsName, id)
		expr -> do
			hsExpr <- dechlorinateExpression expr
			return
				( D.Access "__CASE__"
				, pureLet [D.ValueDef (D.PatTuple ["__CASE__"]) hsExpr]
				)
	let dechlorinateLeaf (exprs, body) = do
		let genGuard = \case
			S.VecCall (S.CallOperator S.OpRange) [exprFrom, exprTo] -> do
				hsRange <- dechlorinateRange exprFrom exprTo
				return $ D.Binary "`elem`" caseExpr hsRange
			expr -> do
				hsExpr <- dechlorinateExpression expr
				return $ D.Binary "==" caseExpr hsExpr
		hsExprs <- mapM genGuard exprs
		hsBody <- dechlorinatePureBody body
		return (foldl1 (D.Binary "||") hsExprs, hsBody)
	hsBodyElse <- dechlorinatePureBody bodyElse
	hsGuardLeafs
		<- (++ [(D.Access "otherwise", hsBodyElse)])
		<$> mapM dechlorinateLeaf leafs
	let hsGuardExpression name
		= pureLet [D.GuardDef (D.PatTuple [name]) hsGuardLeafs]
		$ D.Access name
	return $ wrap (hsGuardExpression "__RES__")

dechlorinatePureVecIfBranch (S.VecIfBranch expr bodyThen bodyElse)
	 =  D.IfExpression
	<$> dechlorinateExpression expr
	<*> dechlorinatePureBody bodyThen
	<*> dechlorinatePureBody bodyElse

dechlorinatePureVecForCycle (S.VecForCycle argIndices name exprFrom exprTo clBody) = do
		hsRange <- dechlorinateRange exprFrom exprTo
		hsBody <- dechlorinatePureBody clBody
		hsArgExpr
			<- D.Tuple
			<$> (map D.Access
				<$> dechlorinateIndicesList argIndices)
		hsFoldLambda <- dechlorinateFoldLambda argIndices name <*> return hsBody
		return $ beta
			[ D.Access "foldl"
			, hsFoldLambda
			, hsArgExpr
			, hsRange
			]

dechlorinatePureStatement
	:: S.VecStatement
	-> (Fail String) D.ValueDef
dechlorinatePureStatement = \case
	S.VecAssign name i expr -> do
		hsExpr <- dechlorinateExpression expr
		hsName <- dechlorinateName name i
		return $ D.ValueDef (D.PatFunc hsName []) hsExpr
	S.VecForStatement retIndices vecForCycle
		 -> D.ValueDef
		<$> (D.PatTuple
			<$> dechlorinateIndicesList retIndices)
		<*> dechlorinatePureVecForCycle vecForCycle
	S.VecIfStatement retIndices vecIfBranch
		 -> D.ValueDef
		<$> (D.PatTuple
			<$> dechlorinateIndicesList retIndices)
		<*> dechlorinatePureVecIfBranch vecIfBranch
	S.VecCaseStatement retIndices vecCaseBranch
		 -> D.ValueDef
		<$> (D.PatTuple
			<$> dechlorinateIndicesList retIndices)
		<*> dechlorinatePureVecCaseBranch vecCaseBranch
	_ -> mzero

beta = foldl1 D.Beta

pureLet [] expr = expr
pureLet defs expr = D.PureLet defs expr

dechlorinateIndicesList
	= mapM (uncurry dechlorinateName)

dechlorinateArgument :: S.VecArgument -> (Fail String) D.Expression
dechlorinateArgument = \case
	S.VecLValue name i -> D.Access <$> dechlorinateName name i
	S.VecRValue expr -> dechlorinateExpression expr

dechlorinateExpression :: S.VecExpression -> (Fail String) D.Expression
dechlorinateExpression = \case
	S.VecPrimary prim -> return $ case prim of
		S.Quote  cs -> D.Quote cs
		S.INumber intSection -> D.INumber intSection
		S.FNumber intSection fracSection
			-> D.FNumber intSection fracSection
		S.ENumber intSection fracSection eSign eSection
			-> D.ENumber intSection fracSection eSign eSection
		S.BTrue  -> D.BTrue
		S.BFalse -> D.BFalse
		S.Void   -> D.Tuple []
	S.VecAccess name i -> D.Access <$> dechlorinateName name i
	S.VecCall callName exprs -> do
		hsExprs <- mapM dechlorinateExpression exprs
		let binary hsOp = case hsExprs of
			hsExpr1:hsExpr2:hsExprs ->
				return $ beta (D.Binary hsOp hsExpr1 hsExpr2 : hsExprs)
			_ -> mzero
		case callName of
			S.CallName name
				-> return
				 $ beta (D.Access (transformName name) : hsExprs)
			S.CallOperator op -> case op of
				S.OpNegate -> do
					case hsExprs of
						[hsExpr1] -> return (D.Negate hsExpr1)
						_ -> mzero
				S.OpShow -> do
					case hsExprs of
						[hsExpr1] -> return (D.Access "show" `D.Beta` hsExpr1)
						_ -> mzero
				S.OpAdd -> binary "+"
				S.OpSubtract -> binary "-"
				S.OpMultiply -> binary "*"
				S.OpDivide -> binary "/"
				S.OpMore -> binary ">"
				S.OpLess -> binary "<"
				S.OpEquals -> binary "=="
				S.OpAnd -> binary "&&"
				S.OpOr -> binary "||"
