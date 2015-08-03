module Implicit.LetReplacer where

import Implicit.Atom
import qualified Implicit.EvaluationMemory as EM

import Lava.Word
import Lava.Vector
import Lava.Recipe
import Lava.Generic
import Lava.Bit
import Lava.Recipe
import Lava.Prelude

data LetReplacer m n =
    LetReplacer {
      reference :: Reg (S (S (S (S n)))),
      contents :: Reg (S (S (S (S n)))),
      occupied :: Reg N1,
      address :: Sig m,
      readValue :: Word (S (S (S (S n)))),
      writeValue :: Sig (S (S (S (S n))))
    }

newLetReplacer :: (N m, N n) => (EM.EvaluationMemory m (S (S (S (S n))))) -> New (LetReplacer m n)
newLetReplacer memory = do
  ref <- newReg
  cont <- newReg
  occ <- newReg

  return $ LetReplacer {
    reference = ref,
    contents = cont,
    occupied = occ,
    address = EM.address memory,
    readValue = EM.output memory,
    writeValue = EM.input memory
  }

letReplace :: (N m, N n) => LetReplacer m n -> Recipe
letReplace lr =
  Seq [
    While (lr!occupied!val!vhead!inv) $
    Seq [
      lr!occupied!val!vhead <&> lr!readValue === lr!reference!val |>
        lr!writeValue <== lr!contents!val
    , lr!occupied!val!vhead!inv <&> lr!readValue!isLet |>
      Seq [
        lr!reference <== lr!readValue
      , lr!incrementAddress
      , Tick
      , lr!contents <== lr!readValue
      , lr!occupied <== 1
      ]
    , Tick
    ]
  , While (lr!readValue!isUnLet!inv) $
    Seq [
      lr!incrementAddress
    , Tick
    ]
  ]

incrementAddress :: (N m, N n) => LetReplacer m n -> Recipe
incrementAddress lr =
  lr!address <== lr!address!val + 1