{-# LANGUAGE LambdaCase #-}
module Frontend.Parser (parse) where

import Control.Applicative
import Control.Monad
import qualified Tr
import qualified Frontend.Token as T
import Frontend.Program
import Success

parse :: [T.Token] -> (Fail String) Program
parse
	= Tr.trap
		(const <$> programTr)
		(guard . isDot)
	where
		isDot (T.Dot:_) = True
		isDot _ = False


-- Useful combinators

expect a = mfilter (==a) Tr.head

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
	<$  expect T.KwVar
	<*> many (varDeclTr
	<*  expect T.Semicolon)

varDeclTr
	 =  VarDecl
	<$> nameTr
	<*  expect T.Colon
	<*> typeTr

typeTr = nameTr >>= \case
	"integer" -> return PasInteger
	"real" -> return PasReal
	"boolean" -> return PasBoolean
	"string"  -> return PasString
	cs -> return $ PasType cs

bodyTr
	 =  Body . fst
	<$  expect T.KwBegin
	<*> Tr.before
		(statementTr <* expect T.Semicolon)
		(expect T.KwEnd)

funcTr
	 =  Func
	<$  expect T.KwFunction
	<*> nameTr
	<*> paramsTr
	<*  expect T.Colon
	<*> typeTr
	<*  expect T.Semicolon
	<*> varsTr
	<*> bodyTr
	<*  expect T.Semicolon

paramsTr
	 =  Vars
	<$  expect T.LParen
	<*> mplus end next
	where
		end = expect T.RParen *> return []
		next
			 =  (:)
			<$> varDeclTr
			<*> mplus (expect T.Semicolon *> next) end

statementTr
	= msum
	[ assignTr
	, executeTr
	, forCycleTr
	]

assignTr
	 =  Assign
	<$> nameTr
	<*  expect T.Assign
	<*> conditionTr

executeTr
	 =  Execute
	<$> nameTr
	<*> mplus argsTr (return [])

forCycleTr
	 = ForCycle
	<$> sodiumTr
	<*  expect T.KwFor
	<*> nameTr
	<*  expect T.Assign
	<*> conditionTr
	<*  expect T.KwTo
	<*> conditionTr
	<*  expect T.KwDo
	<*> bodyTr

sodiumTr
	=  expect T.SodiumSpecial
	*> (fst <$> Tr.before nameTr (expect T.RBrace))

conditionTr = do
	expr1 <- expressionTr
	mOp <- optional opTr
	return $ case mOp of
		Nothing -> expr1
		Just op -> op expr1
	where
		opTr = do
			op <- Tr.head >>= \case
				T.Suck -> return OpLess
				T.Blow -> return OpMore
				T.EqSign -> return OpEquals
				_ -> mzero
			expr2 <- expressionTr
			return $ flip (Binary op) expr2

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

primTr
	= msum
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
	*> mplus end next
	where
		end = expect T.RParen *> return []
		next
			 =  (:)
			<$> conditionTr
			<*> mplus (expect T.Comma *> next) end

accessTr
	 =  Access
	<$> nameTr

numberTr = Tr.head >>= \case
	T.Number cs -> return $ Number cs
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
