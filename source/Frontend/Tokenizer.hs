module Frontend.Tokenizer (tokenize) where

import Control.Applicative
import Control.Monad
import qualified Data.Char as C
import qualified Tr
import Frontend.Token
import Success

tokenize :: String -> (Fail String) [Token]
tokenize = Tr.trap
	(const <$> concat <$> many tokenTr)
	(guard . null)

tokenTr :: Tr.Tr String (Fail String) [Token]
tokenTr = msum
	[ [] <$ whitespace
	, [SodiumSpecial]
		<$ char '{'
		<* optional whitespace
		<* mapM char "#SODIUM"
	, [] <$ comment
	, [RBrace] <$ char '}'
	, [LParen] <$ char '('
	, [RParen] <$ char ')'
	, [LSqBrace] <$ char '['
	, [RSqBrace] <$ char ']'
	, [DoubleDot] <$ mapM char ".."
	, [Dot] <$ char '.'
	, [Comma] <$ char ','
	, [Plus]  <$ char '+'
	, [Minus] <$ char '-'
	, [Slash] <$ char '/'
	, [Assign] <$ mapM char ":="
	, [Colon] <$ char ':'
	, [Asterisk] <$ char '*'
	, [Semicolon] <$ char ';'
	, [EqSign] <$ char '='
	, [Suck] <$ char '<'
	, [Blow] <$ char '>'
	, (\cs -> [Number cs]) <$> some (mfilter C.isDigit Tr.head)
	, let letter = C.toLower <$> mfilter C.isAlphaNum Tr.head
	  in mangle <$> some (letter <|> char '_')
	, (\cs -> [Quote cs]) <$> quote
	] where
		char c = mfilter (==c) Tr.head
		whitespace = some (mfilter C.isSpace Tr.head)
		comment = char '{' <* let
			comment' = void (char '}') <|> void (unit *> comment')
			unit = void comment <|> void Tr.head
			in comment'
		quote = let
			qmark = '\''
			quote' = ((:) <$> unit <*> quote') <|> ("" <$ char qmark)
			unit = (char qmark *> char qmark) <|> (mfilter (/=qmark) Tr.head)
			in char qmark *> quote'
		mangle cs = maybe [Name cs] id (lookup cs keywords)
		keywords =
			[ ("var", [KwVar])
			, ("begin", [KwBegin])
			, ("end", [KwEnd])
			, ("for", [KwFor])
			, ("to", [KwTo])
			, ("do", [KwDo])
			, ("function", [KwFunction])
			, ("true", [KwTrue])
			, ("false", [KwFalse])
			, ("and", [KwAnd])
			, ("or", [KwOr])
			, ("if", [KwIf])
			, ("then", [KwThen])
			, ("else", [KwElse])
			, ("case", [KwCase])
			, ("of", [KwOf])
			]
