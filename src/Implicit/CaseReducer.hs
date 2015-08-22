{-# LANGUAGE NamedFieldPuns #-}
module Implicit.CaseReducer where

import Implicit.Atom
import qualified Implicit.EvaluationMemory as EM

import Lava.Bit
import Lava.Recipe
import Lava.Vector
import Lava.Word
import Lava.Generic
import Lava.Ram

import Debug.Trace

data CaseReducer m n =
    CaseReducer {
      reference  :: Reg n,
      contents :: Reg (S (S (S (S (S n))))),
      state   :: Reg N3,
      -- 0 : store case reference and delete
      -- 1 : store scrutinee and delete
      -- 2 : compare case
      address    :: Reg m,
      memory  :: Word (S (S (S (S (S n))))),
      writeData :: Sig (S (S (S (S (S n))))),
      writeEn :: Sig N1
    } deriving Show

newCaseReducer :: (N m, N n) => [Integer] -> New (CaseReducer m n)
newCaseReducer program = do
  reference <- newReg
  contents <- newReg
  state <- newReg
  address <- newReg

  writeData <- newSig
  writeEn <- newSig

  let ramInputs = RamInputs {
        ramData = writeData!val,
        ramAddress = address!val,
        ramWrite = writeEn!val!vhead
      }
      memory = EM.evaluationMemory ramInputs program

  return $ CaseReducer {
    reference,
    contents,
    state,
    address,
    memory,
    writeData,
    writeEn
  }

caseReduce :: (N m, N n) => CaseReducer m n -> Recipe
caseReduce cr = Seq [
  --  cr!bindCaseReference
  --, cr!bindCaseContents
  --, cr!trimArms
  --, cr!ignoreArm
  --, cr!unbindCase
  cr!incrementAddress
  , Tick
  ]

bindCaseReference :: (N n) => CaseReducer m n -> Recipe
bindCaseReference cr = cr!state!val === 0 <&> cr!memory!isCase |>
  Seq [
    cr!reference <== cr!memory!contentBits
  , cr!writeData <== cr!memory!markDelete
  , cr!writeEn <== 1
  , cr!state <== 1
  ]

bindCaseContents :: (N m, N n) => CaseReducer m n -> Recipe
bindCaseContents cr = cr!state!val === 1 |>
  Seq [
    cr!contents <== cr!memory
  , cr!writeData <== cr!memory!markDelete
  , cr!writeEn <== 1
  , cr!state <== 2
  ]

trimArms :: (N m, N n) => CaseReducer m n -> Recipe
trimArms cr = cr!state!val === 2 |>
  Seq [
      cr!writeData <== cr!memory!markDelete
    , cr!writeEn <== 1
    , cr!memory!isArm <&> cr!reference!val === cr!memory!contentBits |>
        cr!state <== 3
    ]

ignoreArm :: (N m, N n) => CaseReducer m n -> Recipe
ignoreArm cr = cr!state!val === 3 |>
  cr!memory!isArm <&> cr!reference!val === cr!memory!contentBits |>
    Seq [
      cr!writeData <== cr!memory!markDelete
    , cr!writeEn <== 1
    , cr!state <== 2
    ]

unbindCase :: (N m, N n) => CaseReducer m n -> Recipe
unbindCase cr = cr!memory!isUnCase <&> cr!reference!val === cr!memory!contentBits |>
  Seq [
    cr!writeData <== cr!memory!markDelete
  , cr!writeEn <== 1
  , cr!state <== 4
  ]

incrementAddress :: (N m, N n) => CaseReducer m n -> Recipe
incrementAddress cr =
  cr!address <== cr!address!val + 1