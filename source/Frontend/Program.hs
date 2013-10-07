module Frontend.Program
	( Program(..)
	, Vars(..)
	, Body(..)
	, VarDecl(..)
	, Statement(..)
	, Expression(..)
	, Term(..)
	, Prim(..)
	, Name
	, PasType(..)
	) where

type Name = String

data Program
	= Program (Maybe Vars) Body
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
	deriving (Show)

data Expression
	= Expression Term
	| ExpressionAdd Term Expression
	| ExpressionSubtract Term Expression
	deriving (Show)

data Term
	= Term Prim
	| TermMultiply Term Prim
	| TermDivide Term Prim
	deriving (Show)

data Prim
	= Access Name
	| Call Name [Expression]
	| Number String
	| Quote String
	deriving (Show)

data PasType
	= PasInteger
	| PasReal
	| PasBoolean
	| PasString
	| PasType Name
	deriving (Show)
