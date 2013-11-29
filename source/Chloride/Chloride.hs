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
	, _funcRet :: ClType
	, _funcBody :: b
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
	| Execute Name [Argument]
	| ForStatement ForCycle
	deriving (Show)

data ForCycle
	= ForCycle
	{ _forClosure :: [Name]
	, _forName :: Name
	, _forFrom :: Expression
	, _forTo :: Expression
	, _forBody :: Body
	} deriving (Show)

data Expression
	= Access Name
	| Call Name [Expression]
	| Number String
	| Quote String
	| Binary Operator Expression Expression
	deriving (Show)

data VecBody
	= VecBody
	{ _vecBodyVars :: Vars
	, _vecBodyStatements :: [VecStatement]
	, _vecBodyIndices :: Indicies
	} deriving (Show)

data VecArgument
	= VecLValue Name
	| VecRValue VecExpression
	deriving (Show)

data VecStatement
	= VecAssign Name Integer VecExpression
	| VecExecute Name [VecArgument]
	| VecForStatement VecForCycle
	deriving (Show)

data VecForCycle
	= VecForCycle
	{ _vecForClosure :: [Name]
	, _vecForName :: Name
	, _vecForFrom :: VecExpression
	, _vecForTo :: VecExpression
	, _vecForBody :: VecBody
	} deriving (Show)

data VecExpression
	= VecAccess Name Integer
	| VecCall Name [VecExpression]
	| VecNumber String
	| VecQuote String
	| VecBinary Operator VecExpression VecExpression
	deriving (Show)

data Operator
	= OpAdd
	| OpSubtract
	| OpMultiply
	| OpDivide
	deriving (Show)

data ClType
	= ClInteger
	| ClDouble
	| ClBoolean
	| ClString
	| ClVoid
	deriving (Show)

type Vars
	= M.Map Name ClType

type Indicies
	= M.Map Name Integer
