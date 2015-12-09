module Implicit.Let where

import Prelude hiding (Word, splitAt)

import Implicit.Atom
import Implicit.AtomType
import Implicit.ShiftRegister
import Implicit.PolyTree
import Implicit.Word
import Implicit.Context.BindWord

import Lava.Bit
import Lava.Vector
import Lava.Ram

data LetReplacer w = LetReplacer {
    input :: Sentence w

  , selected :: Word w
  , lookupTable :: Vec (S w) Bit
  
  , depth :: Word w
  , ramOut :: Word w

  , output :: Sentence w
}

letReplace :: N w => Word w -> Word w
letReplace input = getElem 2 $ match ((A Let, onLet) <:+> (Many (Any, onAny) ((A In, onIn) <:+> Nil))) (serialToParallel 3 input)
                    where onLet w = bindWord "selected" w
                          onAny w = undefined
                          onIn w = undefined
                     {-LetReplacer {
                        input = input
                      , selected = bind
                      , lookupTable = ram [] Width9 $
                                     RamInputs {
                                       ramData = depth
                                     , ramAddress = contents $ selected bind
                                     , ramWrite = inv $ deleted bind }
                      , depth = mapContents (+1) . getElem 0 $ serialToParallel 1 $ depth
                      , ramOut = undefined
                      , output = 0 <:+> Many 0 <:+> Nil }-}