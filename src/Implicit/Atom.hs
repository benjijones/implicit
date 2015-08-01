module Implicit.Atom where

import Lava.Bit
import Lava.Vector

import Lava.Prelude

data Atom n =
    Data (Word n)

  -- branching
  | Case (Word n)
  | Arm (Word n)

  -- let expressions
  | Let (Word n)
  | In
  | LetRef (Word n)
  | UnLet (Word n)
  
  deriving Show

atomToVec :: (N n) => Atom n -> Word (S (S (S n)))
atomToVec (Data value) = low  +> low  +> low  +> value -- 000(0|1)^n
atomToVec (Case arm)   = low  +> high +> low  +> arm   -- 010(0|1)^n
atomToVec (Arm arm)    = low  +> high +> high +> arm   -- 011(0|1)^n
atomToVec (Let ref)    = high +> low  +> low  +> ref   -- 100(0|1)^n
atomToVec In           = high +> low  +> high +> boolsToWord (repeat False)
atomToVec (LetRef ref) = high +> high +> low  +> ref   -- 110(0|1)^n