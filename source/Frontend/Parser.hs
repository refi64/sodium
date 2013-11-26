{-# LANGUAGE LambdaCase #-}
module Frontend.Parser (parse) where

import Control.Applicative
import Control.Monad
import qualified Tr
import qualified Frontend.Token as T
import Frontend.Program
import Debug.Trace

parse :: [T.Token] -> Maybe Program
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

-- BUG? Pascal is case-insensitive
-- and this function is case-sensitive
typeTr = nameTr >>= \case
	"integer" -> return PasInteger
	"real" -> return PasReal
	"boolean" -> return PasBoolean
	"string" -> return PasString
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
	<*> expressionTr

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
	<*> expressionTr
	<*  expect T.KwTo
	<*> expressionTr
	<*  expect T.KwDo
	<*> bodyTr

sodiumTr
	=  expect T.SodiumSpecial
	*> (fst <$> Tr.before nameTr (expect T.RBrace))

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

primTr
	= msum
	[ Call <$> nameTr <*> argsTr
	, accessTr
	, numberTr
	, quoteTr
	, enclosedTr
	]

enclosedTr
	=  expect T.LParen
	*> expressionTr
	<* expect T.RParen

argsTr
	=  expect T.LParen
	*> mplus end next
	where
		end = expect T.RParen *> return []
		next
			 =  (:)
			<$> expressionTr
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

nameTr :: Tr.Tr [T.Token] Maybe Name
nameTr = Tr.head >>= \case
	T.Name cs -> return cs 
	_ -> mzero
