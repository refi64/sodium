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

programTr = Program <$> msum [varsTr, return (Vars [])] <*> bodyTr

expect a = mfilter (==a) Tr.head

varsTr = do
	expect T.KwVar
	Vars <$> many varDeclTr

varDeclTr = VarDecl <$> nameTr <* expect T.Colon <*> typeTr <* expect T.Semicolon

typeTr = nameTr >>= \case
	"integer" -> return PasInteger
	"real" -> return PasReal
	"boolean" -> return PasBoolean
	"string" -> return PasString
	cs -> return $ PasType cs

bodyTr = do
	expect T.KwBegin
	Body . fst <$> Tr.before
		statementTr
		(expect T.KwEnd)

statementTr
	= msum
	$ map (<* expect T.Semicolon)
	[assignTr, executeTr, forCycleTr]

assignTr = do
	cs <- nameTr
	expect T.Assign
	expr <- expressionTr
	return $ Assign cs expr

executeTr = msum
	[ callTr Execute
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

expressionTr = sepr termTr $
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

primTr = msum [callTr Call, accessTr, numberTr, quoteTr, enclosedTr]

callTr :: (Name -> [Expression] -> a) -> Tr.Tr [T.Token] Maybe a
callTr f = do
	cs <- nameTr
	expect T.LParen
	let end = Tr.head >>= \case
		T.RParen -> return $ f cs []
		_ -> mzero
	mplus end (f cs <$> argsTr)

argsTr = do
	expr <- expressionTr
	Tr.head >>= \case
		T.Comma -> (expr:) <$> argsTr
		T.RParen -> return [expr]
		_ -> mzero

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
