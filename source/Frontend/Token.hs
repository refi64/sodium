module Frontend.Token (Token(..)) where

data Token
	= KwVar
	| KwBegin
	| KwEnd
	| KwFor
	| KwTo
	| KwDo
	| KwFunction
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
	| SodiumSpecial
	| RBrace
	deriving (Eq, Show)
