module Chloride.Chloride where

import qualified Data.Map as M

data Name
	= Name String
	| NameMain
	| NameUnique Name
	deriving (Eq, Ord, Show)

data Program
	= Program [Func Body]
	deriving (Show)

data VecProgram
	= VecProgram [Func VecBody]
	deriving (Show)

data Func b
	= Func
	{ _funcName :: Name
	, _funcParams :: Vars
	, _funcRetType :: ClType
	, _funcBody :: b
	} deriving (Show)

data Body
	= Body
	{ _bodyVars :: Vars
	, _bodyStatements :: [Statement]
	, _bodyResults :: [Expression]
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
	deriving (Show)

data ForCycle
	= ForCycle
	{ _forClosure :: [Name]
	, _forName :: Name
	, _forFrom :: Expression
	, _forTo :: Expression
	, _forBody :: Body
	} deriving (Show)

data IfBranch
	= IfBranch
	{ _ifClosure :: [Name]
	, _ifExpr :: Expression
	, _ifThen :: Body
	, _ifElse :: Body
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
	deriving (Show)

data VecForCycle
	= VecForCycle
	{ _vecForArgIndices :: Indices
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

data VecExpression
	= VecAccess Name Integer
	| VecCall CallName [VecExpression]
	| VecPrimary Literal
	deriving (Show)

data Literal
	= Number String
	| Quote  String
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
	| OpShow
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
