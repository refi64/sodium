module Backend.Program
	( Name
	, Program(..)
	, Def(..)
	, ValueDef(..)
	, Expression(..)
	, DoStatement(..)
	, HsType(..)
	, Pattern(..)
	) where

type Name = String

data HsType
	= HsUnit
	| HsIO HsType
	| HsType Name
	deriving (Show)

data Program
	= Program [Def] [String]
	deriving (Show)

type Def = ValueDef

data ValueDef
	= ValueDef Pattern Expression
	| GuardDef Pattern [(Expression, Expression)]
	deriving (Show)

data Expression
	= Access Name
	| Lambda [Pattern] Expression
	| Beta Expression Expression
	-- TODO: Sections instead of Binary
	| Binary Name Expression Expression
	| Negate Expression
	| Tuple [Expression]
	| Quote String
	| INumber String
	| FNumber String String
	| ENumber String String Bool String
	| BTrue | BFalse
	| Typed Expression HsType
	| DoExpression [DoStatement]
	| PureLet [ValueDef] Expression
	| Range Expression Expression
	| IfExpression Expression Expression Expression
	deriving (Show)

data DoStatement
	= DoBind Pattern Expression
	| DoLet  Name Expression
	| DoExecute Expression
	deriving (Show)

data Pattern
	= PatTuple [Name]
	| PatFunc Name [Name]
	deriving (Show)
