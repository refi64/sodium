module Sodium.Frontend.Token (Token(..)) where

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
	| KwCase
	| KwOf
	| LParen
	| RParen
	| Semicolon
	| Comma
	| Dot
	| DoubleDot
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
	| INumber String
	| FNumber String String
	| ENumber String String Bool String
	| Quote String
	| SodiumSpecial
	| RBrace
	| LSqBrace
	| RSqBrace
	deriving (Eq, Show)
