module Sodium.Frontend.Parser (parse) where

import Prelude hiding (head)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Sodium.Tr (trapGuard, head, before, fallback, expect)
import qualified Sodium.Frontend.Token as T
import Sodium.Frontend.Program

parse :: [T.Token] -> Either String Program
parse = trapGuard programTr isDot where
	isDot (T.Dot:_) = True
	isDot _ = False


-- Useful combinators

sepr elemTr opTr = tr where
	tr = elemTr >>= next
	next a = fallback a $ opTr <*> return a <*> tr

sepl elemTr opTr = elemTr >>= next where
	next a = fallback a $ (opTr <*> return a <*> elemTr) >>= next


-- Syntactic definitions

programTr
	 =  Program
	<$> many funcTr
	<*> varsTr
	<*> bodyTr

varsTr
	 =  fallback []
	 $  expect T.KwVar
	 *> many (varDeclTr
	<*  expect T.Semicolon)

varDeclTr
	 =  VarDecl
	<$> varNamesTr
	<*> typeTr

varNamesTr = end <|> next where
	end = expect T.Colon *> return []
	next
		 =  (:)
		<$> nameTr
		<*> (expect T.Comma *> next <|> end)

typeTr = nameTr >>= \case
	"integer" -> return PasInteger
	"longint" -> return PasLongInt
	"real" -> return PasReal
	"boolean" -> return PasBoolean
	"string"  -> return PasString
	"array" -> do
		expect T.KwOf
		PasArray <$> typeTr
	cs -> return $ PasType cs

bodyStatementTr = bodyTr <|> (wrap <$> statementTr) where
	wrap statement = Body [statement]

bodyTr
	 =  Body . snd
	<$  expect T.KwBegin
	<*> before
		(statementTr <* expect T.Semicolon)
		(expect T.KwEnd)

funcTr
	 =  Func
	<$  expect T.KwFunction
	<*> nameTr
	<*> fallback [] paramsTr
	<*  expect T.Colon
	<*> typeTr
	<*  expect T.Semicolon
	<*> varsTr
	<*> bodyTr
	<*  expect T.Semicolon

paramsTr
	 =  expect T.LParen
	 *> (end <|> next)
	where
		end = expect T.RParen *> return []
		next
			 =  (:)
			<$> varDeclTr
			<*> (expect T.Semicolon *> next <|> end)

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
	<*  expect T.Assign
	<*> conditionTr

executeTr
	 =  Execute
	<$> nameTr
	<*> fallback [] argsTr

forCycleTr
	 = ForCycle
	<$  expect T.KwFor
	<*> nameTr
	<*  expect T.Assign
	<*> conditionTr
	<*  expect T.KwTo
	<*> conditionTr
	<*  expect T.KwDo
	<*> bodyStatementTr

ifBranchTr
	 =  IfBranch
	<$  expect T.KwIf
	<*> conditionTr
	<*> thenClause
	<*> optional elseClause
	where
		thenClause
			 = expect T.KwThen
			*> bodyStatementTr
		elseClause
			 = expect T.KwElse
			*> bodyStatementTr

caseBranchTr
	 =  CaseBranch
	<$  expect T.KwCase
	<*> conditionTr
	<*  expect T.KwOf
	<*> many (caseClause
	<*  expect T.Semicolon)
	<*> optional (elseClause
	<*  expect T.Semicolon)
	<*  expect T.KwEnd
	where
		caseClause
			 =  (,)
			<$> caseOneTr
			<*> bodyStatementTr
		elseClause
			 =  expect T.KwElse
			 *> bodyStatementTr

sodiumTr
	=  expect T.SodiumSpecial
	*> (snd <$> before nameTr (expect T.RBrace))

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
	$ head >>= \case
		T.Suck -> return OpLess
		T.Blow -> return OpMore
		T.EqSign -> return OpEquals
		_ -> mzero

rangeTr
	= sepnTr expressionTr expressionTr
	$ head >>= \case
		T.DoubleDot -> return OpRange
		_ -> mzero

caseOneTr = end <|> next where
	end = expect T.Colon *> return []
	next
		 =  (:)
		<$> rangeTr
		<*> (expect T.Comma *> next <|> end)

expressionTr = sepl termTr $
	head >>= \case
		T.Plus  -> return (Binary OpAdd)
		T.Minus -> return (Binary OpSubtract)
		T.KwOr  -> return (Binary OpOr)
		_ -> mzero

termTr = sepl primTr $
	head >>= \case
		T.Asterisk -> return (Binary OpMultiply)
		T.Slash -> return (Binary OpDivide)
		T.KwAnd -> return (Binary OpAnd)
		_ -> mzero

unaryTr = fallback id opTr where
	opTr = head >>= \case
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
	=  expect T.LParen
	*> conditionTr
	<* expect T.RParen

argsTr
	=  expect T.LParen
	*> (end <|> next)
	where
		end = expect T.RParen *> return []
		next
			 =  (:)
			<$> conditionTr
			<*> (expect T.Comma *> next <|> end)

accessTr
	 =  Access
	<$> nameTr

numberTr = head >>= \case
	T.INumber intSection -> return $
		INumber intSection
	T.FNumber intSection fracSection -> return $
		FNumber intSection fracSection
	T.ENumber intSection fracSection eSign eSection -> return $
		ENumber intSection fracSection eSign eSection
	_ -> mzero

quoteTr = head >>= \case
	T.Quote cs -> return $ Quote cs
	_ -> mzero

boolTr = head >>= \case
	T.KwTrue  -> return $ BTrue
	T.KwFalse -> return $ BFalse
	_ -> mzero

nameTr = head >>= \case
	T.Name cs -> return cs 
	_ -> mzero
