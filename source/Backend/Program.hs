module Backend.Program
	( Name
	, Program(..)
	, Def(..)
	, Expression(..)
	, DoStatement(..)
	, HsType(..)
	) where

type Name = String

data HsType
	= HsInteger
	| HsDouble
	| HsBoolean
	| HsString
	| HsIO HsType
	| HsType Name
	deriving (Show)

data Program
	= Program [Def]
	deriving (Show)

data Def
	= ValueDef Name Expression
	deriving (Show)

data Expression
	= Access Name
	| Lambda [Name] Expression
	| Beta Expression Expression
	| Binary Name Expression Expression
	| Tuple [Expression]
	| Quote String
	| Number String
	| Typed Expression HsType
	| DoExpression [DoStatement]
	| Range Expression Expression
	deriving (Show)

data DoStatement
	= DoBind Name Expression
	| DoLet  Name Expression
	| DoExecute Expression
	deriving (Show)
