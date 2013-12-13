module Frontend.Program
	( Program(..)
	, Vars(..)
	, Body(..)
	, Func(..)
	, VarDecl(..)
	, Statement(..)
	, Expression(..)
	, Operator(..)
	, Name
	, PasType(..)
	) where

type Name = String

data Program
	= Program [Func] Vars Body
	deriving (Show)

data Vars
	= Vars [VarDecl]
	deriving (Show)

data Body
	= Body [Statement]
	deriving (Show)

data Func
	= Func Name Vars PasType Vars Body
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
	| BTrue | BFalse
	| Binary Operator Expression Expression
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
	deriving (Show)

data PasType
	= PasInteger
	| PasReal
	| PasBoolean
	| PasString
	| PasType Name
	deriving (Show)
