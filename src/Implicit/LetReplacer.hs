module Implicit.LetReplacer where

import Lava.Word
import Lava.Vector
import Lava.Recipe

data LetReplacer n =
    LetReplacer {
      contents :: Word n,
      state :: Word N1
    }

letReplace :: LetReplacer -> Recipe