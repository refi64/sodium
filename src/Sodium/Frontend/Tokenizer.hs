module Sodium.Frontend.Tokenizer (tokenize) where

import Control.Applicative
import Control.Monad
import qualified Data.Char as C
import qualified Sodium.Tr as Tr
import Sodium.Frontend.Token
import Sodium.Success

tokenize :: String -> (Fail String) [Token]
tokenize = Tr.trapGuard
	(many $ many ignored *> token)
	(all C.isSpace)

token :: Tr.Tr String (Fail String) Token
token = msum
	[ LParen <$ Tr.expect '('
	, RParen <$ Tr.expect ')'
	, LSqBrace <$ Tr.expect '['
	, RSqBrace <$ Tr.expect ']'
	, Plus  <$ Tr.expect '+'
	, Minus <$ Tr.expect '-'
	, Asterisk <$ Tr.expect '*'
	, Slash <$ Tr.expect '/'
	, Comma <$ Tr.expect ','
	, Semicolon <$ Tr.expect ';'
	, EqSign <$ Tr.expect '='
	, Suck <$ Tr.expect '<'
	, Blow <$ Tr.expect '>'
	, Tr.expect '.' *> Tr.fallback Dot (DoubleDot <$ Tr.expect '.')
	, Tr.expect ':' *> Tr.fallback Colon (Assign <$ Tr.expect '=')
	, number
	, name
	, quote
	]

ignored = void whitespace <|> comment

name = mangle <$> some (letter <|> Tr.expect '_') where
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
		= Tr.fallback True
		$ (True <$ Tr.expect '+') <|> (False <$ Tr.expect '-')
	intSection <- some digit
	Tr.fallback (INumber intSection) $ do
		Tr.expect '.'
		fracSection <- some digit
		Tr.fallback (FNumber intSection fracSection) $ do
			Tr.expect 'e'
			eSign <- sign
			eSection <- some digit
			return (ENumber intSection fracSection eSign eSection)

quote = Quote <$> (qmark *> quote') where
	qmark = Tr.expect '\''
	quote'
		 =  qmark *> Tr.fallback "" (next qmark)
		<|> next Tr.head
	next x = (:) <$> x <*> quote'

comment = Tr.expect '{' *> comment' where
	comment'
		 =  void (Tr.expect '}')
		<|> (comment <|> void Tr.head) *> comment'

whitespace = mfilter C.isSpace Tr.head

letter = C.toLower <$> mfilter C.isAlphaNum Tr.head

digit = mfilter C.isDigit Tr.head

-- Not used for now
sodiumSpecial
	= SodiumSpecial
	<$ Tr.expect '{'
	<* many whitespace
	<* mapM Tr.expect "#SODIUM"
