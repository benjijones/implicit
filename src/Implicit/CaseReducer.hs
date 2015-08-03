module Implicit.CaseReducer where

import Implicit.Atom

import Lava.Bit
import Lava.Recipe
import Lava.Vector
import Lava.Word
import Lava.Generic

import Debug.Trace

data CaseReducer n =
    CaseReducer {
      reference  :: Reg (S (S (S (S n)))),
      occupied   :: Reg N1,
      address    :: Sig (S (S (S (S n)))),
      readValue  :: Word (S (S (S (S n)))),
      writeValue :: Sig (S (S (S (S n))))
    }

newCaseReducer :: (N n) => New (CaseReducer n)
newCaseReducer = do
  ref <- newReg
  occ <- newReg

  addr <- newSig
  write <- newSig

  return $ CaseReducer {
    reference = ref,
    occupied = occ,
    address = addr,
    readValue = delay (low +> low +> low +> high +> vrepeat low) (vrepeat low),
    writeValue = write
  }

caseReduce :: (N n) => CaseReducer n -> Recipe
caseReduce cr =
  While (cr!occupied!val!vhead!inv) $
  Seq [
  --cr!occupied!val!vhead <&> cr!readValue === cr!reference!val |>
  cr!occupied!val!vhead!inv <&> cr!readValue!isCase |>
    Seq [
      cr!incrementAddress
    , Tick
    , cr!readValue!isData |>
      Seq [
        cr!reference <== cr!readValue
      , cr!occupied <== 1
      ]
    ]
  , Tick
  ]

deleteArm :: (N n) => CaseReducer n -> Recipe
deleteArm cr =
  Seq [
    While (cr!readValue!isUnArm!inv) $
      Seq [
        cr!writeValue <== cr!readValue!markDelete
      , Tick
      , cr!incrementAddress
    ]
  , cr!writeValue <== cr!readValue!markDelete
  ]

incrementAddress :: (N n) => CaseReducer n -> Recipe
incrementAddress cr =
  cr!address <== cr!address!val + 1