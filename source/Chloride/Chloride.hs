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

data Argument e
	= LValue Name
	| RValue e
	deriving (Show)

data Statement
	= Assign Name Expression
	| Execute Name [Argument Expression]
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
	| Primary Literal
	| Binary Operator Expression Expression
	deriving (Show)

data VecBody
	= VecBody
	{ _vecBodyVars :: Vars
	, _vecBodyStatements :: [VecStatement]
	, _vecBodyIndices :: Indices
	} deriving (Show)

data VecStatement
	= VecAssign Name Integer VecExpression
	| VecExecute Name [Argument VecExpression]
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
	| VecPrimary Literal
	| VecBinary Operator VecExpression VecExpression
	deriving (Show)

data Literal
	= Number String
	| Quote  String
	deriving (Show)

-- TODO: Operator is basically a
-- function name, so Binary and Call
-- can be merged
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

type Indices
	= M.Map Name Integer
