module Sodium (translate) where

import Sodium.Frontend.Chlorinate  (chlorinate)
import Sodium.Frontend.Parser      (parse)
import Sodium.Frontend.Tokenizer   (tokenize)
import Sodium.Backend.Dech         (dechlor)
import Sodium.Backend.Render       (render)
import Sodium.Chloride.Vectorizer  (vectorize)
import Sodium.Chloride.Flatten     (flatten)
import Sodium.Chloride.IOMagic     (uncurse)
import Sodium.Chloride.Pattern     (sub)

translate :: String -> String
translate
	= render
	. dechlor
	. sub
	. vectorize
	. uncurse
	. flatten
	. chlorinate
	. parse
	. tokenize
