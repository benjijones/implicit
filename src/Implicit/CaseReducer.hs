{-# LANGUAGE NamedFieldPuns #-}
module Implicit.CaseReducer where

import Prelude hiding (Word)

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
--      newState :: Sig N3,
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
--  newState <- newSig
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
--    newState,
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
  , cr!trimArm
  , cr!compareArm
  , cr!deleteArm
  , cr!skipArm
  , cr!unbindCase
--  , cr!updateState
  ]

bindCaseReference :: (N n) => CaseReducer m n -> Recipe
bindCaseReference cr = cr!isUnbound <&> cr!memoryIn!isCase |>
  Seq [
    cr!reference <== cr!memoryIn!contentBits
  , cr!delete <== 1
  , cr!state <== 1
  ]

bindCaseContents :: (N m, N n) => CaseReducer m n -> Recipe
bindCaseContents cr = cr!isBinding |>
  Seq [
    cr!contents <== cr!memoryIn
  , cr!delete <== 1
  , cr!state <== 2
  ]

trimArm :: (N m, N n) => CaseReducer m n -> Recipe
trimArm cr = cr!isBound |>
  Seq [
      cr!delete <== 1
    , cr!memoryIn!isArm <&> cr!reference!val === cr!memoryIn!contentBits |>
        cr!state <== 3
    ]

compareArm :: (N m, N n) => CaseReducer m n -> Recipe
compareArm cr = cr!isCompare |>
  Seq [
    cr!delete <== 1
  , cr!contents!val === cr!memoryIn |>
      cr!state <== 5
  , cr!contents!val === cr!memoryIn |>
      cr!state <== 4
  ]

deleteArm :: (N m, N n) => CaseReducer m n -> Recipe
deleteArm cr = cr!isDelete |>
  Seq [
    cr!delete <== 1
  , cr!memoryIn!isArm <&> cr!reference!val === cr!memoryIn!contentBits |>
      cr!state <== 3
  ]

skipArm :: (N m, N n) => CaseReducer m n -> Recipe
skipArm cr = cr!isSkip |>
  Seq [
    cr!memoryIn!isArrow <&> cr!reference!val === cr!memoryIn!contentBits |>
      cr!delete <== 1
  , cr!memoryIn!isArm <&> cr!reference!val === cr!memoryIn!contentBits |>
    Seq [
      cr!delete <== 1
    , cr!state <== 3
    ]
  ]

unbindCase :: (N m, N n) => CaseReducer m n -> Recipe
unbindCase cr = cr!memoryIn!isUnCase <&> cr!reference!val === cr!memoryIn!contentBits |>
  Seq [
    cr!delete <== 1
  , cr!state <== 0
  ]

--updateState :: (N m, N n) => CaseReducer m n -> Recipe
--updateState cr = cr!state <== cr!newState!val

isUnbound :: CaseReducer m n -> Bit
isUnbound = (=== 0) . val . state

isBinding :: CaseReducer m n -> Bit
isBinding = (=== 1) . val . state

isBound :: CaseReducer m n -> Bit
isBound = (=== 2) . val . state

isCompare :: CaseReducer m n -> Bit
isCompare = (=== 3) . val . state

isDelete :: CaseReducer m n -> Bit
isDelete = (=== 4) . val . state

isSkip :: CaseReducer m n -> Bit
isSkip = (=== 5) . val . state