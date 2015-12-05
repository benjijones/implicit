module Implicit.ShiftRegister where

import Prelude hiding (Word)

import Implicit.PolyTree
import Implicit.Word

import qualified Lava.Generic as G
import Lava.Vector

serialToParallel :: (N w) => Integer -> Word w -> Sentence w
serialToParallel 0 input = input <:+> Nil
serialToParallel n input = serialToParallel (n-1) (delay input) `mappend` (input <:+> Nil) 