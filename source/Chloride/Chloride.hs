module Chloride.Chloride where

data Name
	= Name String
	| NameMain
	deriving (Show)

data Program
	= Program [Func]
	deriving (Show)

data Func
	= Func
	{ _funcName :: Name
	, _funcParams :: Vars
	, _funcRet :: ClType
	, _funcBody :: Body
	} deriving (Show)

data Body
	= Body
	{ _bodyVars :: Vars
	, _bodyStatements :: [Statement]
	} deriving (Show)

data Statement
	= Assign Name Expression
	| Execute Name [Expression]
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
	= [(Name, ClType)]
