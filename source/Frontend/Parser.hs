{-# LANGUAGE LambdaCase #-}
module Frontend.Parser (parse) where

import Control.Applicative
import Control.Monad
import qualified Tr
import qualified Frontend.Token as T
import Frontend.Program
import Debug.Trace

parse :: [T.Token] -> Maybe Program
parse = Tr.runTrap builder

builder :: Tr.Trap [T.Token] Maybe Program
builder = Tr.trap (const <$> programTr) $ Tr.Trap (guard . isDot) where
	isDot (T.Dot:_) = True
	isDot _ = False

programTr
	=   Program
	<$> many funcTr
	<*> varsTr
	<*> bodyTr

expect a = mfilter (==a) Tr.head

varsTr
	=   flip mplus (return $ Vars [])
	$   Vars
	<$  expect T.KwVar
	<*> many (varDeclTr
	<*  expect T.Semicolon)

varDeclTr
	=   VarDecl
	<$> nameTr
	<*  expect T.Colon
	<*> typeTr

typeTr = nameTr >>= \case
	"integer" -> return PasInteger
	"real" -> return PasReal
	"boolean" -> return PasBoolean
	"string" -> return PasString
	cs -> return $ PasType cs

bodyTr = do
	expect T.KwBegin
	Body . fst <$> Tr.before
		(statementTr <* expect T.Semicolon)
		(expect T.KwEnd)

funcTr = do
	expect T.KwFunction
	name <- nameTr
	params <- Vars <$> paramsTr
	expect T.Colon
	retType <- typeTr
	expect T.Semicolon
	Func name params retType <$> varsTr <*> bodyTr <* expect T.Semicolon

paramsTr = expect T.LParen *> mplus end next where
	end = expect T.RParen *> return []
	next = (:) <$> varDeclTr <*> mplus (expect T.Semicolon *> next) end

statementTr
	= msum
	[ assignTr
	, executeTr
	, forCycleTr
	]

assignTr = do
	cs <- nameTr
	expect T.Assign
	expr <- expressionTr
	return $ Assign cs expr

executeTr = msum
	[ Execute <$> nameTr <*> argsTr
	, do
		name <- nameTr
		return $ Execute name []
	]

forCycleTr = do
	expect T.SodiumSpecial
	names <- fst <$> Tr.before
		nameTr
		(expect T.RBrace)
	expect T.KwFor
	name <- nameTr
	expect T.Assign
	exprFrom <- expressionTr
	expect T.KwTo
	exprTo <- expressionTr
	expect T.KwDo
	body <- bodyTr
	return $ ForCycle names name exprFrom exprTo body

expressionTr = sepl termTr $
	Tr.head >>= \case
		T.Plus -> return (Binary OpAdd)
		T.Minus -> return (Binary OpSubtract)
		_ -> mzero

termTr = sepl primTr $
	Tr.head >>= \case
		T.Asterisk -> return (Binary OpMultiply)
		T.Slash -> return (Binary OpDivide)
		_ -> mzero

sepr elemTr opTr = tr where
	tr = do
		elem <- elemTr
		(opTr <*> return elem <*> tr) `mplus` return elem

sepl elemTr opTr =
	let next a = (opTr <*> return a <*> elemTr >>= next) `mplus` return a
	in elemTr >>= next

primTr
	= msum
	[ Call <$> nameTr <*> argsTr
	, accessTr
	, numberTr
	, quoteTr
	, enclosedTr]

argsTr = expect T.LParen *> mplus end next where
	end = expect T.RParen *> return []
	next = (:) <$> expressionTr <*> mplus (expect T.Comma *> next) end

accessTr = Access <$> nameTr

numberTr = Tr.head >>= \case
	T.Number cs -> return $ Number cs
	_ -> mzero

quoteTr = Tr.head >>= \case
	T.Quote cs -> return $ Quote cs
	_ -> mzero

enclosedTr = do
	expect T.LParen
	expr <- expressionTr
	expect T.RParen
	return expr

nameTr :: Tr.Tr [T.Token] Maybe Name
nameTr = Tr.head >>= \case
	T.Name cs -> return cs 
	_ -> mzero
