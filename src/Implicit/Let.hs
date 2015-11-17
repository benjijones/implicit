module Implicit.Let where

import Prelude hiding (Word, splitAt)

import Implicit.Atom
import Implicit.AtomType
import Implicit.Match

import Lava.Word
import Lava.Bit
import Lava.Vector

data LetReplacer = LetReplacer {
                      contents :: Word N1 AtomN
                    , output :: Word N3 AtomN }

instance Generic LetReplacer where
  generic (LetReplacer contents output) = cons LetReplacer >< contents >< output

letReplace :: Word N3 AtomN -> LetReplacer -> LetReplacer
letReplace input prev = match (A Let +> Any +> A In +> vempty) input
                          LetReplacer {
                            contents = at n1 input,
                            output = empty
                          }
                          LetReplacer {
                            contents = contents prev,
                            output = input
                          }