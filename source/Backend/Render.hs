{-# LANGUAGE LambdaCase #-}

module Backend.Render
	( render
	) where

import Data.List (intersperse)
import qualified Text.PrettyPrint as P
import Backend.Program

vsep = foldr (P.$+$) P.empty

render :: Program -> String
render (Program defs imports)
	= P.render
	$ vsep
	$  map renderImport imports
	++ map renderDef defs

renderImport cs
	= P.text "import"
	P.<+> P.text cs

renderDef (ValueDef name expr)
	= P.hsep
	[ renderName name
	, P.text "="
	, renderExpression expr
	]

renderExpression (Access name)
	= renderName name

renderExpression (Lambda names expr)
	= P.hsep
	[ P.hcat
		[P.text "\\"
		, P.hsep $ map renderName names
		]
	, P.text "->"
	, renderExpression expr
	]

renderExpression (Beta expr1 expr2)
	= P.hsep
	$ map P.parens
	$ map renderExpression
	$ [expr1, expr2]

renderExpression (Binary op expr1 expr2)
	= P.hsep
	[ P.parens $ renderExpression expr1
	, renderName op
	, P.parens $ renderExpression expr2
	]

renderExpression (Tuple exprs)
	= P.parens
	$ P.hsep
	$ P.punctuate P.comma
	$ map renderExpression
	$ exprs

renderExpression (Typed expr t)
	= P.hsep
	[ P.parens $ renderExpression expr
	, P.text "::"
	, renderType t
	]

renderExpression (DoExpression statements)
	= P.text "do" P.$+$ (P.nest 4 $ vsep $ map renderStatement $ statements)

renderExpression (Range exprFrom exprTo)
	= P.brackets
	$ P.hcat
	[ renderExpression exprFrom
	, P.text ".."
	, renderExpression exprTo
	]

renderExpression (Quote cs)
	= P.text (show cs)

renderExpression (Number cs)
	= P.text cs

renderName
	= P.text

renderType = \case
	HsInteger -> P.text "Integer"
	HsIO t -> P.hsep [P.text "IO", renderType t]

renderStatement (DoBind name expr)
	= P.hsep
	[ renderName name
	, P.text "<-"
	, renderExpression expr
	]

renderStatement (DoLet name expr)
	= P.hsep
	[ P.text "let"
	, renderName name
	, P.text "="
	, renderExpression expr
	]

renderStatement (DoExecute expr)
	= renderExpression expr
