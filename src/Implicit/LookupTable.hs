module Implicit.LookupTable where

import Lava.Recipe
import Lava.Vector

data LookupTable =
  LookupTable {
    offset :: Reg N9
    
  }