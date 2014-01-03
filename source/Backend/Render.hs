{-# LANGUAGE LambdaCase #-}

module Backend.Render
	( render
	) where

import Data.List (intersperse)
import qualified Text.PrettyPrint as P
import Backend.Program
import Success

vsep = foldr (P.$+$) P.empty

render :: Program -> (Fail String) String
render (Program defs imports)
	= return
	$ P.render
	$ vsep
	$  map renderImport imports
	++ map renderDef defs

renderImport cs
	= P.text "import"
	P.<+> P.text cs

renderDef (ValueDef (PatFunc name names) expr)
	= P.hsep
	[ renderName name
	, P.hsep $ map renderName names
	, P.text "="
	, renderExpression expr
	]

renderDef (ValueDef pat expr)
	= P.hsep
	[ renderPattern pat
	, P.text "="
	, renderExpression expr
	]

renderDef (GuardDef pat leafs)
	= renderPattern pat
	P.$+$
		( P.nest 4
		$ vsep
		$ map renderLeaf
		$ leafs
		)
	where
		renderLeaf (expr1, expr2)
			= P.hsep
			[ P.text "|"
			, renderExpression expr1
			, P.text "="
			, renderExpression expr2
			]

renderExpression (Access name)
	= renderName name

renderExpression (Lambda pats expr)
	= P.hsep
	[ P.hcat
		[ P.text "\\"
		, P.hsep $ map renderPattern pats
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

renderExpression (Tuple [Access name])
	= renderName name

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
	= P.text "do"
	P.$+$
		( P.nest 4
		$ vsep
		$ map renderStatement
		$ statements
		)

renderExpression (PureLet valueDefs expr)
	= P.text "let"
	P.$+$
		( P.nest 4
		$ vsep
		$ map renderDef
		$ valueDefs
		)
	P.$+$
		(P.text "in" P.<+> renderExpression expr)

renderExpression (Range exprFrom exprTo)
	= P.brackets
	$ P.hcat
	[ renderExpression exprFrom
	, P.text ".."
	, renderExpression exprTo
	]

renderExpression (IfExpression expr bodyThen bodyElse)
	= (P.text "if" P.<+> renderExpression expr)
	P.$+$ (P.text "then" P.<+> renderExpression bodyThen)
	P.$+$ (P.text "else" P.<+> renderExpression bodyElse)

renderExpression (Quote cs)
	= P.text (show cs)

renderExpression (Number cs)
	= P.text cs

renderExpression BTrue
	= P.text "True"

renderExpression BFalse
	= P.text "False"

renderName
	= P.text

renderType = \case
	HsType cs  -> P.text cs
	HsUnit -> P.text "()"
	HsIO t -> P.hsep [P.text "IO", renderType t]

renderStatement (DoBind pat expr)
	= P.hsep
	[ renderPattern pat
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

renderPattern (PatTuple [name])
	= renderName name

renderPattern (PatTuple names)
	= P.parens
	$ P.hsep
	$ P.punctuate P.comma
	$ map renderName
	$ names
