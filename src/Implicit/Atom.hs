module Implicit.Atom where

import Lava.Bit
import Lava.Vector


import Lava.Prelude

data Atom n =
    Data (Integer)

  -- branching
  | Case
  | Arm (Integer)

  -- let expressions
  | Let (Integer)
  | In
  | LetRef (Integer)
  | UnLet (Integer)

  deriving Show

atomToVec :: (N n) => Atom n -> Word (S (S (S n)))
atomToVec (Data val)   = low  +> low  +> low  +> fromInteger val -- 000(0|1)^n
atomToVec Case         = low  +> low  +> high +> 0               -- 001(0)^n
atomToVec (Arm arm)    = low  +> high +> high +> fromInteger arm -- 011(0|1)^n
atomToVec (Let ref)    = high +> low  +> low  +> fromInteger ref -- 100(0|1)^n
atomToVec In           = high +> low  +> high +> 0               -- 101(0)^n
atomToVec (LetRef ref) = high +> high +> low  +> fromInteger ref -- 110(0|1)^n
atomToVec (UnLet ref)  = high +> high +> high +> fromInteger ref -- 111(0|1)^n

isLet :: (N n) => Word (S (S (S n))) -> Bit
isLet w = (w `vat` n0) <&> inv (w `vat` n1) <&> inv (w `vat` n2)