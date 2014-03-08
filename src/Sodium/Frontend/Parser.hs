module Sodium.Frontend.Parser (parse) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Sodium.Tr as Tr
import qualified Sodium.Frontend.Token as T
import Sodium.Frontend.Program
import Sodium.Success

parse :: [T.Token] -> (Fail String) Program
parse = Tr.trapGuard programTr isDot where
	isDot (T.Dot:_) = True
	isDot _ = False


-- Useful combinators

sepr elemTr opTr = tr where
	tr = elemTr >>= next
	next a = flip mplus (return a) $ opTr <*> return a <*> tr

sepl elemTr opTr = elemTr >>= next where
	next a = flip mplus (return a) $ (opTr <*> return a <*> elemTr) >>= next


-- Syntactic definitions

programTr
	 =  Program
	<$> many funcTr
	<*> varsTr
	<*> bodyTr

varsTr
	 =  flip mplus (return $ Vars [])
	 $  Vars
	<$  Tr.expect T.KwVar
	<*> many (varDeclTr
	<*  Tr.expect T.Semicolon)

varDeclTr
	 =  VarDecl
	<$> varNamesTr
	<*> typeTr

varNamesTr = mplus end next where
	end = Tr.expect T.Colon *> return []
	next
		 =  (:)
		<$> nameTr
		<*> mplus (Tr.expect T.Comma *> next) end

typeTr = nameTr >>= \case
	"integer" -> return PasInteger
	"longint" -> return PasLongInt
	"real" -> return PasReal
	"boolean" -> return PasBoolean
	"string"  -> return PasString
	"array" -> do
		Tr.expect T.KwOf
		PasArray <$> typeTr
	cs -> return $ PasType cs

bodyStatementTr = mplus bodyTr (wrap <$> statementTr) where
	wrap statement = Body [statement]

bodyTr
	 =  Body . fst
	<$  Tr.expect T.KwBegin
	<*> Tr.before
		(statementTr <* Tr.expect T.Semicolon)
		(Tr.expect T.KwEnd)

funcTr
	 =  Func
	<$  Tr.expect T.KwFunction
	<*> nameTr
	<*> (paramsTr <|> return (Vars []))
	<*  Tr.expect T.Colon
	<*> typeTr
	<*  Tr.expect T.Semicolon
	<*> varsTr
	<*> bodyTr
	<*  Tr.expect T.Semicolon

paramsTr
	 =  Vars
	<$  Tr.expect T.LParen
	<*> mplus end next
	where
		end = Tr.expect T.RParen *> return []
		next
			 =  (:)
			<$> varDeclTr
			<*> mplus (Tr.expect T.Semicolon *> next) end

statementTr
	= msum
	[ assignTr
	, executeTr
	, forCycleTr
	, ifBranchTr
	, caseBranchTr
	]

assignTr
	 =  Assign
	<$> nameTr
	<*  Tr.expect T.Assign
	<*> conditionTr

executeTr
	 =  Execute
	<$> nameTr
	<*> mplus argsTr (return [])

forCycleTr
	 = ForCycle
	<$  Tr.expect T.KwFor
	<*> nameTr
	<*  Tr.expect T.Assign
	<*> conditionTr
	<*  Tr.expect T.KwTo
	<*> conditionTr
	<*  Tr.expect T.KwDo
	<*> bodyStatementTr

ifBranchTr
	 =  IfBranch
	<$  Tr.expect T.KwIf
	<*> conditionTr
	<*> thenClause
	<*> optional elseClause
	where
		thenClause
			 = Tr.expect T.KwThen
			*> bodyStatementTr
		elseClause
			 = Tr.expect T.KwElse
			*> bodyStatementTr

caseBranchTr
	 =  CaseBranch
	<$  Tr.expect T.KwCase
	<*> conditionTr
	<*  Tr.expect T.KwOf
	<*> many (caseClause
	<*  Tr.expect T.Semicolon)
	<*> optional (elseClause
	<*  Tr.expect T.Semicolon)
	<*  Tr.expect T.KwEnd
	where
		caseClause
			 =  (,)
			<$> caseOneTr
			<*> bodyStatementTr
		elseClause
			 =  Tr.expect T.KwElse
			 *> bodyStatementTr

sodiumTr
	=  Tr.expect T.SodiumSpecial
	*> (fst <$> Tr.before nameTr (Tr.expect T.RBrace))

sepnTr elem1Tr elem2Tr opTr = do
	elem1 <- elem1Tr
	mOpf <- optional opfTr
	return $ fromMaybe id mOpf elem1
	where
		opfTr = do
			op <- opTr
			elem2 <- elem2Tr
			return $ flip (Binary op) elem2

conditionTr
	= sepnTr expressionTr expressionTr
	$ Tr.head >>= \case
		T.Suck -> return OpLess
		T.Blow -> return OpMore
		T.EqSign -> return OpEquals
		_ -> mzero

rangeTr
	= sepnTr expressionTr expressionTr
	$ Tr.head >>= \case
		T.DoubleDot -> return OpRange
		_ -> mzero

caseOneTr = mplus end next where
	end = Tr.expect T.Colon *> return []
	next
		 =  (:)
		<$> rangeTr
		<*> mplus (Tr.expect T.Comma *> next) end

expressionTr = sepl termTr $
	Tr.head >>= \case
		T.Plus  -> return (Binary OpAdd)
		T.Minus -> return (Binary OpSubtract)
		T.KwOr  -> return (Binary OpOr)
		_ -> mzero

termTr = sepl primTr $
	Tr.head >>= \case
		T.Asterisk -> return (Binary OpMultiply)
		T.Slash -> return (Binary OpDivide)
		T.KwAnd -> return (Binary OpAnd)
		_ -> mzero

unaryTr = opTr <|> return id where
	opTr = Tr.head >>= \case
		T.Plus -> return (Unary UOpPlus)
		T.Minus -> return (Unary UOpNegate)
		_ -> mzero

primTr
	 =  unaryTr
	<*> msum
	[ Call <$> nameTr <*> argsTr
	, accessTr
	, numberTr
	, quoteTr
	, boolTr
	, enclosedTr
	]

enclosedTr
	=  Tr.expect T.LParen
	*> conditionTr
	<* Tr.expect T.RParen

argsTr
	=  Tr.expect T.LParen
	*> mplus end next
	where
		end = Tr.expect T.RParen *> return []
		next
			 =  (:)
			<$> conditionTr
			<*> mplus (Tr.expect T.Comma *> next) end

accessTr
	 =  Access
	<$> nameTr

numberTr = Tr.head >>= \case
	T.INumber intSection -> return $
		INumber intSection
	T.FNumber intSection fracSection -> return $
		FNumber intSection fracSection
	T.ENumber intSection fracSection eSign eSection -> return $
		ENumber intSection fracSection eSign eSection
	_ -> mzero

quoteTr = Tr.head >>= \case
	T.Quote cs -> return $ Quote cs
	_ -> mzero

boolTr = Tr.head >>= \case
	T.KwTrue  -> return $ BTrue
	T.KwFalse -> return $ BFalse
	_ -> mzero

nameTr :: Tr.Tr [T.Token] (Fail String) Name
nameTr = Tr.head >>= \case
	T.Name cs -> return cs 
	_ -> mzero
