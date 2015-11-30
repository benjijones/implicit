module Implicit.Let where

import Prelude hiding (Word, splitAt)

import Implicit.Atom
import Implicit.AtomType
import Implicit.PolyTree
import Implicit.Word

import Lava.Bit
import Lava.Vector

data LetReplacer w = LetReplacer {
    input :: Sentence w
  , reference :: Word w
  , 
  , letContents :: Sentence w
  , output :: Sentence w
}

newLetReplacer input = LetReplacer {
                         input = input
                       , reference = newWord 0 0
                       , letContents = newWord 0 0
                       , output = input }

letReplace :: LetReplacer w -> LetReplacer w
letReplace prev = case match (A Let <:+> Many Any <:+> Nil) (input prev) of
                    Cons bind (Many bound Nil) -> LetReplacer {
                            input = input prev,
                            letContents = slice n1 n4 (input prev),
                            output = 0 <:+> Many 0 <:+> Nil
                          } ${-}
                        match (A LetRef +> n3 <:*> Any) (input prev)
                          LetReplacer {
                            input = input prev,
                            contents = contents prev,
                            output = contents prev `append` empty
                          }-}
                          LetReplacer {
                            input = input prev,
                            contents = contents prev,
                            output = (input prev)
                          }