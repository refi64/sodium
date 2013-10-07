{-# LANGUAGE LambdaCase #-}

module Backend.Render
	( render
	) where

import Data.List (intersperse)
import Backend.Program

roundBrace = ("("++) . (++")")

render :: Program -> String
render (Program defs) = unlines $ map renderDef defs

renderDef (ValueDef name expr)
	= unwords
	[ renderName name
	, "="
	, renderExpression expr
	]

renderExpression (Access name)
	= renderName name

renderExpression (Lambda names expr)
	= roundBrace
	$ unwords
	[ concat 
		["\\"
		, unwords $ map renderName names
		]
	, "->"
	, renderExpression expr
	]

renderExpression (Beta expr1 expr2)
	= roundBrace
	$ unwords
	$ map renderExpression
	$ [expr1, expr2]

renderExpression (Binary op expr1 expr2)
	= roundBrace
	$ unwords
	[ renderExpression expr1
	, renderName op
	, renderExpression expr2
	]

renderExpression (Tuple exprs)
	= roundBrace
	$ concat
	$ intersperse ","
	$ map renderExpression
	$ exprs

renderExpression (Typed expr t)
	= roundBrace
	$ unwords
	[ renderExpression expr
	, "::"
	, renderType t
	]

renderExpression (Quote cs)
	= show cs

renderExpression (Number cs)
	= cs

renderName cs
	= cs

renderType = \case
	HsInteger -> "Integer"
	HsIO t -> unwords ["IO", renderType t]
