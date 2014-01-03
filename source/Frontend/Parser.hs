{-# LANGUAGE LambdaCase #-}
module Frontend.Parser (parse) where

import Control.Applicative
import Control.Monad
import Data.Maybe
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
	<$> varNamesTr
	<*> typeTr

varNamesTr
	= mplus end next
	where
		end = expect T.Colon *> return []
		next
			 =  (:)
			<$> nameTr
			<*> mplus (expect T.Comma *> next) end

typeTr = nameTr >>= \case
	"integer" -> return PasInteger
	"real" -> return PasReal
	"boolean" -> return PasBoolean
	"string"  -> return PasString
	cs -> return $ PasType cs

bodyStatementTr
	= mplus bodyTr (wrap <$> statementTr)
	where
		wrap statement = Body [statement]

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
	<*> (paramsTr <|> return (Vars []))
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
	<*> mplus argsTr (return [])

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
	*> (fst <$> Tr.before nameTr (expect T.RBrace))

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

caseOneTr
	= mplus end next
	where
		end = expect T.Colon *> return []
		next
			 =  (:)
			<$> rangeTr
			<*> mplus (expect T.Comma *> next) end

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
