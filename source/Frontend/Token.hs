module Frontend.Token (Token(..)) where

data Token
	= KwVar
	| KwBegin
	| KwEnd
	| LParen
	| RParen
	| Semicolon
	| Comma
	| Dot
	| Plus
	| Minus
	| Assign
	| Asterisk
	| Slash
	| Colon
	| Name String
	| Number String
	| Quote String
	deriving (Eq, Show)
