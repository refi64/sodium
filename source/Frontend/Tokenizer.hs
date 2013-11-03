module Frontend.Tokenizer (tokenize) where

import Control.Applicative
import Control.Monad
import qualified Data.Char as C
import qualified Tr
import Frontend.Token

tokenize :: String -> Maybe [Token]
tokenize = Tr.runTrap gather

gather :: Tr.Trap String Maybe [Token]
gather = Tr.trap (const <$> concat <$> many tokenTr) $ Tr.Trap (guard . null)

tokenTr :: Tr.Tr String Maybe [Token]
tokenTr = msum
	[ [] <$ mfilter C.isSpace Tr.head
	, [SodiumSpecial]
		<$ char '{'
		<* many (mfilter C.isSpace Tr.head)
		<* str "#SODIUM"
	, [RBrace] <$ char '}'
	, [LParen] <$ char '('
	, [RParen] <$ char ')'
	, [Dot] <$ char '.'
	, [Comma] <$ char ','
	, [Plus]  <$ char '+'
	, [Minus] <$ char '-'
	, [Slash] <$ char '/'
	, [Assign] <$ str ":="
	, [Colon] <$ char ':'
	, [Asterisk] <$ char '*'
	, [Semicolon] <$ char ';'
	, (\cs -> [Number cs]) <$> some (mfilter C.isDigit Tr.head)
	, let letter = C.toLower <$> mfilter C.isAlphaNum Tr.head
	  in mangle <$> some (letter `mplus` char '_')
	, (\(cs, _) -> [Quote cs])
		<$  char '\''
		-- TODO: Pascal-style escaping
		<*> let escaped = char '\\' *> Tr.head
		    in mplus escaped Tr.head `Tr.before` char '\''
	] where
		char c = mfilter (==c) Tr.head
		str = mapM char
		mangle cs = maybe [Name cs] id (lookup cs keywords)
		keywords =
			[ ("var", [KwVar])
			, ("begin", [KwBegin])
			, ("end", [KwEnd])
			, ("for", [KwFor])
			, ("to", [KwTo])
			, ("do", [KwDo])
			, ("function", [KwFunction])
			]
