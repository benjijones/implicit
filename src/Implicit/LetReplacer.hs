module Implicit.LetReplacer where

import Implicit.Atom

import Lava.Word
import Lava.Vector
import Lava.Recipe
import Lava.Generic
import Lava.Bit
import Lava.Recipe
import Lava.Prelude

data LetReplacer n =
    LetReplacer {
      reference :: Reg (S (S (S n))),
      contents :: Reg (S (S (S n))),
      occupied :: Reg N1,
      address :: Sig (S (S (S n))),
      readValue :: Word (S (S (S n))),
      writeValue :: Sig (S (S (S n)))
    } deriving Show

letReplace :: (N n) => LetReplacer n -> Recipe
letReplace lr =
  While (lr!occupied!val!vhead!inv) $
  Seq [
    lr!occupied!val!vhead <&> lr!readValue === lr!reference!val |>
      lr!writeValue <== lr!contents!val
  , lr!occupied!val!vhead!inv <&> lr!readValue!isLet |>
      Seq [
        lr!reference <== lr!readValue
      , lr!address <== lr!address!val + 1
      , Tick
      , lr!contents <== lr!readValue
      , lr!occupied <== (high +> vempty)
      ]
  , Tick
  ]

newLetReplacer :: (N n) => New (LetReplacer n)
newLetReplacer = do
  ref <- newReg
  cont <- newReg
  occ <- newReg

  addr <- newSig
  write <- newSig

  return $ LetReplacer {
    reference = ref,
    contents = cont,
    occupied = occ,
    address = addr,
    readValue = high +> low +> low +> vrepeat low,
    writeValue = write
  }

