module Implicit.Atom where

import Lava.Bit
import Lava.Vector
import Lava.Binary

import Lava.Prelude

data Atom n =
    Data (Integer)

  -- branching
  | Case (Integer)
  | Arm (Integer)

  -- let expressions
  | Let (Integer)
  | In
  | LetRef (Integer)
  | UnLet (Integer)

  deriving Show

atomToVec :: (N n) => n -> Atom n -> Word (S (S (S n)))
atomToVec n (Data val)   = low  +> low  +> low  +> fromInteger val -- 000(0|1)^n
atomToVec n (Case arm)   = low  +> high +> low  +> fromInteger arm -- 010(0|1)^n
atomToVec n (Arm arm)    = low  +> high +> high +> fromInteger arm -- 011(0|1)^n
atomToVec n (Let ref)    = high +> low  +> low  +> fromInteger ref -- 100(0|1)^n
atomToVec n In           = high +> low  +> high +> 0               -- 101(0)^n
atomToVec n (LetRef ref) = high +> high +> low  +> fromInteger ref -- 110(0|1)^n
atomToVec n (UnLet ref)  = high +> high +> high +> fromInteger ref -- 111(0|1)^n