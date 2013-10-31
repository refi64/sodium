module Frontend.Program
	( Program(..)
	, Vars(..)
	, Body(..)
	, VarDecl(..)
	, Statement(..)
	, Expression(..)
	, Operator(..)
	, Name
	, PasType(..)
	) where

type Name = String

data Program
	= Program Vars Body
	deriving (Show)

data Vars
	= Vars [VarDecl]
	deriving (Show)

data Body
	= Body [Statement]
	deriving (Show)

data VarDecl
	= VarDecl Name PasType
	deriving (Show)

data Statement
	= Assign Name Expression
	| Execute Name [Expression]
	| ForCycle [Name] Name Expression Expression Body
	deriving (Show)

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

data PasType
	= PasInteger
	| PasReal
	| PasBoolean
	| PasString
	| PasType Name
	deriving (Show)
