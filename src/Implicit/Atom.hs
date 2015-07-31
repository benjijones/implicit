module Implicit.Atom where

import Lava.Bit
import Lava.Vector

data Atom n =
    Data (Vec n Bit)
  | Case (Vec n Bit)
  | Arm (Vec n Bit)
  | Let (Vec n Bit)
  | Ref (Vec n Bit)

atomToVec :: (N n) => Atom n -> Vec (S (S (S n))) Bit
atomToVec (Data value) = low  +> low  +> low  +> value -- 000(0|1)^n
atomToVec (Case arm)   = low  +> high +> low  +> arm   -- 010(0|1)^n
atomToVec (Arm arm)    = low  +> high +> high +> arm   -- 011(0|1)^n
atomToVec (Let ref)    = high +> low  +> low  +> ref   -- 100(0|1)^n
atomToVec (Ref ref)    = high +> high +> low  +> ref   -- 110(0|1)^n