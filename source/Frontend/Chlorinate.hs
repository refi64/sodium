{-# LANGUAGE LambdaCase #-}
 
module Frontend.Chlorinate
	( chlorinate
	) where

import Control.Monad
import Control.Applicative
-- S for Src, D for Dest
import qualified Frontend.Program  as S
import qualified Chloride.Chloride as D
import qualified Data.Map as M

chlorinate :: S.Program -> Maybe D.Program
chlorinate (S.Program funcs vars body)
	= do
		clMain <- do
			clBody <- chlorinateVB chlorinateName vars body []
			return $ D.Func
				D.NameMain
				M.empty
				D.ClVoid
				clBody
		clFuncs <- mapM
			chlorinateFunc funcs
		return $ D.Program (clMain:clFuncs)

chlorinateVB nameHook (S.Vars vardecls) (S.Body statements) results
	= do
		clVars <- mapM chlorinateVarDecl vardecls
		clStatements <- mapM (chlorinateStatement nameHook) statements
		clResults <- map D.Access <$> mapM nameHook results
		return $ D.Body (M.fromList clVars) clStatements clResults

chlorinateFunc (S.Func name (S.Vars params) pasType vars body)
	 = do
		clName <- chlorinateName name
		clParams <- M.fromList <$> mapM chlorinateVarDecl params
		clRetType <- chlorinateType pasType
		clRetName <- nameHook name
		let enclose body
			= body
			{ D._bodyVars = M.union
				(D._bodyVars body)
				(M.singleton clRetName clRetType)
			}
		clBody <- enclose <$> chlorinateVB nameHook vars body [name]
		return $ D.Func clName clParams clRetType clBody
	where
		nameHook cs =
			if cs == name
				then D.NameUnique <$> chlorinateName cs
				else chlorinateName cs

chlorinateName name
	= return $ D.Name name

chlorinateVarDecl (S.VarDecl name pasType)
	 =  (,)
	<$> chlorinateName name
	<*> chlorinateType pasType

chlorinateType = \case
	S.PasInteger -> return D.ClInteger
	S.PasReal    -> return D.ClDouble
	S.PasBoolean -> return D.ClBoolean
	S.PasString  -> return D.ClString
	S.PasType cs -> mzero

chlorinateStatement nameHook = \case
	S.Assign name expr
		 -> D.Assign
		<$> nameHook name
		<*> chlorinateExpr nameHook expr
	S.Execute name exprs
		 -> D.Execute
		<$> (D.ExecuteName <$> chlorinateName name)
		<*> mapM (chlorinateArgument nameHook) exprs
	S.ForCycle closure name fromExpr toExpr body
		-> D.ForStatement <$> do
			clClosure <- mapM nameHook closure
			clName <- nameHook name
			clFromExpr <- chlorinateExpr nameHook fromExpr
			clToExpr <- chlorinateExpr nameHook toExpr
			clBody <- chlorinateVB nameHook (S.Vars []) body closure
			return $ D.ForCycle clClosure clName clFromExpr clToExpr clBody

chlorinateArgument nameHook = \case
	S.Access name -> D.LValue <$> nameHook name
	expr -> D.RValue <$> chlorinateExpr nameHook expr

chlorinateExpr nameHook = \case
	S.Access name
		 -> D.Access
		<$> nameHook name
	S.Call name exprs
		 -> D.Call
		<$> (D.CallName <$> chlorinateName name)
		<*> mapM (chlorinateExpr nameHook) exprs
	S.Number cs
		-> return
		 $ D.Primary
		 $ D.Number cs
	S.Quote cs
		-> return
		 $ D.Primary
		 $ D.Quote cs
	S.Binary op x y
		 -> D.Call
		<$> (D.CallOperator <$> chlorinateOp op)
		<*> mapM (chlorinateExpr nameHook) [x, y]

chlorinateOp = \case
	S.OpAdd -> return D.OpAdd
	S.OpSubtract -> return D.OpSubtract
	S.OpMultiply -> return D.OpMultiply
	S.OpDivide -> return D.OpDivide
