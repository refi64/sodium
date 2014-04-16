{-# LANGUAGE TemplateHaskell #-}
module Sodium.Chloride.Program where

import Control.Lens (prism', Simple, Prism)
import Control.Lens.TH
import qualified Data.Map as M

data Name
	= Name String
	| NameMain
	| NameUnique Name
	deriving (Eq, Ord, Show)

data Program
	= Program
	{ _programFuncs :: [Func]
	} deriving (Show)

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
	| Execute Operator [Argument]
	| ForStatement ForCycle
	| MultiIfStatement MultiIfBranch
	-- TODO: Use statements instead of bodies
	-- in other statements
	| BodyStatement Body
	deriving (Show)

data ForCycle
	= ForCycle
	{ _forName :: Name
	, _forRange :: Expression
	, _forBody :: Body
	} deriving (Show)

data MultiIfBranch
	= MultiIfBranch
	{ _multiIfLeafs :: [(Expression, Body)]
	, _multiIfElse  :: Body
	} deriving (Show)

data Expression
	= Access Name
	| Call Operator [Expression]
	| Primary Literal
	deriving (Show)

data VecBody
	= VecBody
	{ _vecBodyVars :: Vars
	, _vecBodyStatements :: [(IndicesList, VecStatement)]
	, _vecBodyResults :: [VecExpression]
	} deriving (Show)

data VecArgument
	= VecLValue Name Index
	| VecRValue VecExpression
	deriving (Show)

data VecStatement
	= VecAssign VecExpression
	| VecExecute Operator [VecArgument]
	| VecForStatement VecForCycle
	| VecMultiIfStatement VecMultiIfBranch
	| VecBodyStatement VecBody
	deriving (Show)

data VecForCycle
	= VecForCycle
	{ _vecForArgIndices :: IndicesList
	, _vecForArgExprs :: [VecExpression]
	, _vecForName :: Name
	, _vecForRange :: VecExpression
	, _vecForBody :: VecBody
	} deriving (Show)

data VecMultiIfBranch
	= VecMultiIfBranch
	{ _vecMultiIfLeafs :: [(VecExpression, VecBody)]
	, _vecMultiIfElse  :: VecBody
	} deriving (Show)

data VecExpression
	= VecAccess Name Index
	| VecFold Operator [VecExpression] VecExpression
	| VecCall Operator [VecExpression]
	| VecPrimary Literal
	deriving (Eq, Show)

data Literal
	= INumber String
	| FNumber String String
	| ENumber String String Bool String
	| Quote String
	| BTrue
	| BFalse
	| Void
	deriving (Eq, Show)

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
	| OpProduct
	| OpSum
	| OpAnd'
	| OpOr'
	| OpPrintLn
	| OpReadLn ClType
	| OpName Name
	deriving (Eq, Show)

data ClType
	= ClInteger
	| ClDouble
	| ClBoolean
	| ClString
	| ClVoid
	deriving (Eq, Show)

data Index
	= Index Integer
	| Immutable
	| Uninitialized
	deriving (Eq, Show)

type Vars
	= M.Map Name ClType

type Indices
	= M.Map Name Index

type IndicesList
	= [(Name, Index)]

bodyEmpty :: Body
bodyEmpty = Body M.empty []

bodySingleton :: Simple Prism Body Statement
bodySingleton
	= prism' (\s -> Body M.empty [s])
	$ \case
		Body vars [statement] | M.null vars -> Just statement
		_ -> Nothing

makeLenses ''FuncSig
makeLenses ''Func
makeLenses ''Body
makeLenses ''ForCycle
makeLenses ''MultiIfBranch
makeLenses ''Program

makeLenses ''VecFunc
makeLenses ''VecBody
makeLenses ''VecForCycle
makeLenses ''VecMultiIfBranch
makeLenses ''VecProgram

makePrisms ''Statement
