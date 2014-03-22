module Sodium (translate) where

import Control.Monad
import Sodium.Frontend.Chlorinate  (chlorinate)
import Sodium.Frontend.Parser      (parse)
import Sodium.Frontend.Tokenizer   (tokenize)
import Sodium.Backend.Dech         (dech)
import Sodium.Backend.Render       (render)
import Sodium.Chloride.Vectorizer  (vectorize)
import Sodium.Chloride.IOMagic     (uncurse)
import Sodium.Chloride.Pattern     (sub)

translate :: String -> Either String String
translate
	 =  return . render
	<=< dech
	<=< return . sub
	<=< vectorize
	<=< uncurse
	<=< chlorinate
	<=< parse
	<=< return . tokenize

