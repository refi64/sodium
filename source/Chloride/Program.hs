module Chloride.Program where

import qualified Data.Map as M

data Name
	= Name String
	| NameMain
	| NameUnique Name
	deriving (Eq, Ord, Show)

data Program
	= Program [Func]
	deriving (Show)

data VecProgram
	= VecProgram [VecFunc]
	deriving (Show)

data FuncSig
	= FuncSig
	{ _funcName :: Name
	, _funcParams :: Vars
	, _funcRetType :: ClType
	} deriving (Show)

data Func
	= Func
	{ _funcSig :: FuncSig
	, _funcBody :: Body
	, _funcResults :: [Expression]
	} deriving (Show)

data VecFunc
	= VecFunc
	{ _vecFuncSig :: FuncSig
	, _vecFuncBody :: VecBody
	} deriving (Show)

data Body
	= Body
	{ _bodyVars :: Vars
	, _bodyStatements :: [Statement]
	} deriving (Show)

data Argument
	= LValue Name
	| RValue Expression
	deriving (Show)

data Statement
	= Assign Name Expression
	| Execute ExecuteName [Argument]
	| ForStatement ForCycle
	| IfStatement IfBranch
	| CaseStatement CaseBranch
	deriving (Show)

data ForCycle
	= ForCycle
	{ _forName :: Name
	, _forFrom :: Expression
	, _forTo :: Expression
	, _forBody :: Body
	} deriving (Show)

data IfBranch
	= IfBranch
	{ _ifExpr :: Expression
	, _ifThen :: Body
	, _ifElse :: Body
	} deriving (Show)

data CaseBranch
	= CaseBranch
	{ _caseExpr :: Expression
	, _caseLeafs :: [([Expression], Body)]
	, _caseElse :: Body
	} deriving (Show)

data Expression
	= Access Name
	| Call CallName [Expression]
	| Primary Literal
	deriving (Show)

data VecBody
	= VecBody
	{ _vecBodyVars :: Vars
	, _vecBodyStatements :: [VecStatement]
	, _vecBodyResults :: [VecExpression]
	} deriving (Show)

data VecArgument
	= VecLValue Name Integer
	| VecRValue VecExpression
	deriving (Show)

data VecStatement
	= VecAssign Name Integer VecExpression
	| VecExecute IndicesList ExecuteName [VecArgument]
	| VecForStatement IndicesList VecForCycle
	| VecIfStatement IndicesList VecIfBranch
	| VecCaseStatement IndicesList VecCaseBranch
	deriving (Show)

data VecForCycle
	= VecForCycle
	{ _vecForArgIndices :: IndicesList
	, _vecForName :: Name
	, _vecForFrom :: VecExpression
	, _vecForTo :: VecExpression
	, _vecForBody :: VecBody
	} deriving (Show)

data VecIfBranch
	= VecIfBranch
	{ _vecIfExpr :: VecExpression
	, _vecIfThen :: VecBody
	, _vecIfElse :: VecBody
	} deriving (Show)

data VecCaseBranch
	= VecCaseBranch
	{ _vecCaseExpr :: VecExpression
	, _vecCaseLeafs :: [([VecExpression], VecBody)]
	, _vecCaseElse :: VecBody
	} deriving (Show)

data VecExpression
	= VecAccess Name Integer
	| VecCall CallName [VecExpression]
	| VecPrimary Literal
	deriving (Show)

data Literal
	= INumber String
	| FNumber String String
	| ENumber String String Bool String
	| Quote String
	| BTrue
	| BFalse
	| Void
	deriving (Show)

data Operator
	= OpAdd
	| OpSubtract
	| OpMultiply
	| OpDivide
	| OpLess
	| OpMore
	| OpEquals
	| OpAnd
	| OpOr
	| OpRange
	| OpElem
	| OpShow
	| OpNegate
	deriving (Show)

data CallName
	= CallName Name
	| CallOperator Operator
	deriving (Show)

data ExecuteName
	= ExecuteName Name
	| ExecuteWrite
	| ExecuteRead ClType
	deriving (Show)

data ClType
	= ClInteger
	| ClDouble
	| ClBoolean
	| ClString
	| ClVoid
	deriving (Eq, Show)

type Vars
	= M.Map Name ClType

type Indices
	= M.Map Name Integer

type IndicesList
	= [(Name, Integer)]
