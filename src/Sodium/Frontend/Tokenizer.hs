{-# LANGUAGE DeriveDataTypeable #-}
module Sodium.Frontend.Tokenizer (tokenize) where

import Prelude hiding (head)
import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Char as C
import Sodium.Tr (head, fallback, expect)
import Sodium.Frontend.Token
import Control.Exception
import Data.Typeable

data TokenizerException
	= TokenizerException String -- Position?
	deriving (Show, Typeable)

instance Exception TokenizerException

tokenize :: String -> [Token]
tokenize [] = []
tokenize cs = case runStateT token' cs of
	Nothing -> throw (TokenizerException cs)
	Just (f, cs) -> f (tokenize cs)

token' :: StateT String Maybe ([Token] -> [Token])
token' = ((:) <$> token) <|> (id <$ ignored)

token :: StateT String Maybe Token
token = msum
	[ LParen <$ expect '('
	, RParen <$ expect ')'
	, LSqBrace <$ expect '['
	, RSqBrace <$ expect ']'
	, Plus  <$ expect '+'
	, Minus <$ expect '-'
	, Asterisk <$ expect '*'
	, Slash <$ expect '/'
	, Comma <$ expect ','
	, Semicolon <$ expect ';'
	, EqSign <$ expect '='
	, Suck <$ expect '<'
	, Blow <$ expect '>'
	, expect '.' *> fallback Dot (DoubleDot <$ expect '.')
	, expect ':' *> fallback Colon (Assign <$ expect '=')
	, number
	, name
	, quote
	]

ignored = void (some whitespace) <|> comment

name = mangle <$> some (letter <|> expect '_') where
	mangle cs = maybe (Name cs) id (lookup cs keywords)
	keywords =
		[ ("var", KwVar)
		, ("begin", KwBegin)
		, ("end", KwEnd)
		, ("for", KwFor)
		, ("to", KwTo)
		, ("do", KwDo)
		, ("function", KwFunction)
		, ("true", KwTrue)
		, ("false", KwFalse)
		, ("and", KwAnd)
		, ("or", KwOr)
		, ("if", KwIf)
		, ("then", KwThen)
		, ("else", KwElse)
		, ("case", KwCase)
		, ("of", KwOf)
		]

number = do
	let sign
		= fallback True
		$ (True <$ expect '+') <|> (False <$ expect '-')
	intSection <- some digit
	fallback (INumber intSection) $ do
		expect '.'
		fracSection <- some digit
		fallback (FNumber intSection fracSection) $ do
			expect 'e'
			eSign <- sign
			eSection <- some digit
			return (ENumber intSection fracSection eSign eSection)

quote = Quote <$> (qmark *> quote') where
	qmark = expect '\''
	quote'
		 =  qmark *> fallback "" (next qmark)
		<|> next head
	next x = (:) <$> x <*> quote'

comment = expect '{' *> comment' where
	comment'
		 =  void (expect '}')
		<|> (comment <|> void head) *> comment'

whitespace = mfilter C.isSpace head

letter = C.toLower <$> mfilter C.isAlphaNum head

digit = mfilter C.isDigit head

-- Not used for now
sodiumSpecial
	= SodiumSpecial
	<$ expect '{'
	<* many whitespace
	<* mapM expect "#SODIUM"
