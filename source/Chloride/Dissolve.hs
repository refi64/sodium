module Chloride.Dissolve
	( dissolve
	) where

import Chloride.Chloride
import Chloride.Vectorizer

dissolve :: Program -> Maybe VecProgram
dissolve (Program funcs) = do
	vecFuncs <- mapM vectorize funcs
	return $ VecProgram vecFuncs
