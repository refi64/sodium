{-# LANGUAGE TemplateHaskell #-}
module Sodium.Chloride.Program where

import Control.Lens.TH
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
	= VecProgram
	{ _vecProgramFuncs :: [VecFunc]
	} deriving (Show)

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
	| MultiIfStatement MultiIfBranch
	| BodyStatement Body
	deriving (Show)

data ForCycle
	= ForCycle
	{ _forName :: Name
	, _forFrom :: Expression
	, _forTo :: Expression
	, _forBody :: Body
	} deriving (Show)

data MultiIfBranch
	= MultiIfBranch
	{ _multiIfLeafs :: [(Expression, Body)]
	, _multiIfElse  :: Body
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
	| VecMultiIfStatement IndicesList VecMultiIfBranch
	| VecBodyStatement IndicesList VecBody
	deriving (Show)

data VecForCycle
	= VecForCycle
	{ _vecForArgIndices :: IndicesList
	, _vecForArgExprs :: [VecExpression]
	, _vecForName :: Name
	, _vecForFrom :: VecExpression
	, _vecForTo :: VecExpression
	, _vecForBody :: VecBody
	} deriving (Show)

data VecMultiIfBranch
	= VecMultiIfBranch
	{ _vecMultiIfLeafs :: [(VecExpression, VecBody)]
	, _vecMultiIfElse  :: VecBody
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

makeLenses ''FuncSig
makeLenses ''Func
makeLenses ''Body
makeLenses ''ForCycle
makeLenses ''MultiIfBranch

makeLenses ''VecFunc
makeLenses ''VecBody
makeLenses ''VecForCycle
makeLenses ''VecProgram