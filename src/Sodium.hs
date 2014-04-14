module Sodium (translate) where

import Sodium.Chloride.Vectorize (vectorize)
import Sodium.Chloride.Flatten   (flatten)
import Sodium.Chloride.IOMagic   (uncurse)
import Sodium.Chloride.Pattern   (sub)
import Sodium.Pascal.Parse   (parse)
import Sodium.Haskell.Render (render)
import qualified  Sodium.Pascal.Convert as P (convert)
import qualified Sodium.Haskell.Convert as H (convert)
import Data.Profunctor

translate :: String -> String
translate = dimap fromPascal toHaskell onChloride where
	fromPascal = P.convert . parse
	toHaskell  = render . H.convert
	onChloride = sub . vectorize . uncurse . flatten
