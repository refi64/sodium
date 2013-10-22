{-# LANGUAGE LambdaCase #-}

module Backend.Render
	( render
	) where

import Control.Applicative (empty)
import Data.List (intersperse)
import Backend.Program

import qualified Language.Haskell.Exts as E

srcLoc = error "NO SRC LOC"-- E.SrcLoc "" 0 0

render :: Program -> String
render (Program defs) = E.prettyPrint md where
	md = E.Module
		srcLoc
		(E.ModuleName "Main")
		empty
		empty
		(Just [E.EVar . E.UnQual . E.name $ "main"])
		empty
		decls
	decls = map renderDef defs

renderDef (ValueDef name expr)
	= E.nameBind srcLoc
		(E.name name)
		(renderExpression expr)

renderExpression (Access name)
	= E.var (E.name name)

renderExpression (Lambda names expr)
	= E.lamE srcLoc
		(map (E.pvar . E.name) names)
		(renderExpression expr)

renderExpression (Beta expr1 expr2)
	= E.app
		(renderExpression expr1)
		(renderExpression expr2)

-- WARNING: I'm not sure if the pretty-printer
-- handles fixities right
renderExpression (Binary op expr1 expr2)
	= flip E.infixApp
		(E.op (E.sym op))
		(renderExpression expr1)
		(renderExpression expr2)

renderExpression (Typed expr t)
	= E.ExpTypeSig srcLoc
		(renderExpression expr)
		(renderType t)

renderExpression (Quote cs)
	= E.strE cs

renderExpression (Number cs)
	= E.intE (read cs)

renderExpression (Tuple [])
	= E.Con (E.Special E.UnitCon)

renderExpression (Tuple exprs)
	= E.Tuple (map renderExpression exprs)

renderType = \case
	HsInteger -> E.TyCon (E.UnQual (E.name "Integer"))
	HsIO t -> E.TyApp
		(E.TyCon (E.UnQual (E.name "IO")))
		(renderType t)

