{-# LANGUAGE NamedFieldPuns #-}
module Implicit.CaseReducer where

import Implicit.Atom
import qualified Implicit.EvaluationMemory as EM

import Lava.Bit
import Lava.Recipe
import Lava.Vector
import Lava.Word
import Lava.Generic

data CaseReducer m n =
    CaseReducer {
      reference  :: Reg n,
      contents :: Reg (S (S (S (S (S n))))),
      state   :: Reg N3,
      -- 0 : store case reference and delete
      -- 1 : store scrutinee and delete
      -- 2 : compare case
      --address    :: Reg m,
      memoryIn  :: Word (S (S (S (S (S n))))),
      delete :: Sig N1
      --writeData :: Sig (S (S (S (S (S n))))),
      --writeEn :: Sig N1
    } deriving Show

newCaseReducer :: (N m, N n) => Word (S (S (S (S (S n))))) -> New (CaseReducer m n)
newCaseReducer mem = do
  reference <- newReg
  contents <- newReg
  state <- newReg
  --address <- newReg

  --writeData <- newSig
  --writeEn <- newSig
  delete <- newSig

{-  let ramInputs = RamInputs {
        ramData = writeData!val,
        ramAddress = address!val,
        ramWrite = writeEn!val!vhead
      }
      memory = EM.evaluationMemory ramInputs program -}

  return $ CaseReducer {
    reference,
    contents,
    state,
    --address,
    memoryIn = mem,
    delete
    --writeData,
    --writeEn
  }

caseReduce :: (N m, N n) => CaseReducer m n -> Recipe
caseReduce cr = Seq [
    cr!bindCaseReference
  , cr!bindCaseContents
  , cr!trimArms
  , cr!deleteArm
  , cr!unbindCase
  ]

bindCaseReference :: (N n) => CaseReducer m n -> Recipe
bindCaseReference cr = cr!isUnbound <&> cr!memoryIn!isCase |>
  Seq [
    cr!reference <== cr!memoryIn!contentBits
  , cr!delete <== 1
  , cr!state <== 1
  ]

bindCaseContents :: (N m, N n) => CaseReducer m n -> Recipe
bindCaseContents cr = cr!state!val === 1 |>
  Seq [
    cr!contents <== cr!memoryIn
  , cr!delete <== 1
  , cr!state <== 2
  ]

trimArms :: (N m, N n) => CaseReducer m n -> Recipe
trimArms cr = cr!state!val === 2 |>
  Seq [
      cr!delete <== 1
    , cr!memoryIn!isArm <&> cr!reference!val === cr!memoryIn!contentBits |>
        cr!state <== 3
    ]

deleteArm :: (N m, N n) => CaseReducer m n -> Recipe
deleteArm cr = cr!state!val === 3 |>
  cr!memoryIn!isArm <&> cr!reference!val === cr!memoryIn!contentBits |>
    Seq [
      cr!delete <== 1
    , cr!state <== 2
    ]

unbindCase :: (N m, N n) => CaseReducer m n -> Recipe
unbindCase cr = cr!memoryIn!isUnCase <&> cr!reference!val === cr!memoryIn!contentBits |>
  Seq [
    cr!delete <== 1
  , cr!state <== 4
  ]

isUnbound :: CaseReducer m n -> Bit
isUnbound = (=== 0) . val . state

isBinding :: CaseReducer m n -> Bit
isBinding = (=== 1) . val . state

isBound :: CaseReducer m n -> Bit
isBound = (=== 2) . val . state


{-incrementAddress :: (N m, N n) => CaseReducer m n -> Recipe
incrementAddress cr =
  cr!address <== cr!address!val + 1-}