module Implicit.Let where

import Prelude hiding (Word, splitAt)

import Implicit.Atom
import Implicit.AtomType
import Implicit.Match
import Implicit.Word

import Lava.Bit
import Lava.Vector

data LetReplacer bindl = LetReplacer {
    input :: Word (S bindl) AtomN
  , contents :: Word bindl AtomN
  , output :: Word (S bindl) AtomN
}

newLetReplacer input = LetReplacer {
                         input = input
                       , contents = 0
                       , output = input }

instance Generic (LetReplacer l) where
  generic (LetReplacer input contents output) = cons LetReplacer >< input >< contents >< output



letReplace :: LetReplacer N3 -> LetReplacer N3
letReplace prev = match (A Let +> n3 <:*> Any) (input prev)
                          LetReplacer {
                            input = input prev,
                            contents = slice n1 n4 (input prev),
                            output = empty
                          } $
                        match (A LetRef +> n3 <:*> Any) (input prev)
                          LetReplacer {
                            input = input prev,
                            contents = contents prev,
                            output = contents prev `append` empty
                          }
                          LetReplacer {
                            input = input prev,
                            contents = contents prev,
                            output = (input prev)
                          }