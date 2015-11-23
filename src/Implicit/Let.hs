module Implicit.Let where

import Prelude hiding (Word, splitAt)

import Implicit.Atom
import Implicit.AtomType
import Implicit.Match

import Lava.Word
import Lava.Bit
import Lava.Vector

data LetReplacer bindl = LetReplacer {
                      contents :: Word bindl AtomN
                    , output :: Word (S bindl) AtomN }

instance Generic (LetReplacer l) where
  generic (LetReplacer contents output) = cons LetReplacer >< contents >< output

letReplace :: Word N4 AtomN -> LetReplacer N3 -> LetReplacer N3
letReplace input prev = match (A Let +> n3 <:*> Any) input
                          LetReplacer {
                            contents = slice n1 n4 input,
                            output = empty
                          } $
                        match (A LetRef +> n3 <:*> Any) input
                          LetReplacer {
                            contents = contents prev,
                            output = contents prev `append` empty
                          }
                          LetReplacer {
                            contents = contents prev,
                            output = input
                          }