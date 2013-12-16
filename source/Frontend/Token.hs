module Frontend.Token (Token(..)) where

data Token
	= KwVar
	| KwBegin
	| KwEnd
	| KwFor
	| KwTo
	| KwDo
	| KwFunction
	| KwTrue
	| KwFalse
	| KwAnd
	| KwOr
	| KwIf
	| KwThen
	| KwElse
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
	| EqSign
	| Suck
	| Blow
	| Name String
	| Number String
	| Quote String
	| SodiumSpecial
	| RBrace
	deriving (Eq, Show)
