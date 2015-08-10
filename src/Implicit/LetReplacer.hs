{-# LANGUAGE NamedFieldPuns #-}
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

import Debug.Trace

data LetReplacer m n =
    LetReplacer {
      reference :: Reg n,
      contents :: Reg (S (S (S (S n)))),
      state :: Reg N2,
      -- 0 : unoccupied, looking for a let
      -- 1 : stored a let binding, now storing the contents
      -- 2 : looking for references
      -- 3 : finished
      address :: Reg m,
      memory :: Word (S (S (S (S n)))),
      writeData :: Sig (S (S (S (S n)))),
      writeEn :: Sig N1
    } deriving Show

newLetReplacer :: (N m, N n) => [Integer] -> New (LetReplacer m n)
newLetReplacer program = do
  reference <- newReg
  contents <- newReg
  state <- newReg
  address <- newReg

  writeData <- newSig
  writeEn <- newSig

      --ramInputs :: RamInputs (S (S (S (S n)))) m
  let ramInputs = RamInputs {
        ramData = writeData!val,
        ramAddress = address!val,
        ramWrite = writeEn!val!vhead
      }
      memory = EM.evaluationMemory ramInputs program

      --writeEn = writeEnable (state!val) (reference!val) memory

      --state = delay 0 (nextState state (reference!val) memory)

  return $ LetReplacer {
    reference,
    contents,
    state,
    address,
    memory,
    writeData,
    writeEn
  }

writeEnable :: Word N2 -> Word n -> Word n -> Bit
writeEnable state reference read =
  (state!isBound) <&> (read === reference)

letReplace :: (N n, N m) => LetReplacer m n -> Recipe
letReplace lr = Seq [
    lr!bindReference
  , lr!bindContents
  , lr!replaceLet
  , lr!unbind
  , lr!incrementAddress
  , lr!state!val === 3 |> lr!address <== 0
  , Tick
  ]

bindReference :: (N m, N n) => LetReplacer n m -> Recipe
bindReference lr = lr!state!val!isUnbound <&> lr!memory!isLet |>
      Seq [
        lr!reference <== lr!memory!contentBits
      , lr!writeData <== lr!memory!markDelete
      , lr!writeEn <== 1
      , lr!state <== 1
      ]

bindContents :: (N m, N n) => LetReplacer n m -> Recipe
bindContents lr = lr!state!val!isBinding |>
      Seq [
        lr!contents <== lr!memory
      , lr!writeData <== lr!memory!markDelete
      , lr!writeEn <== 1
      , lr!state <== 2
      ]

-- need to delete "In" cell-
-- might need another state bit,
-- it would useful to have anyways.

replaceLet :: (N m, N n) => LetReplacer n m -> Recipe
replaceLet lr = lr!state!val!isBound <&> lr!memory!isLetRef |>
      Seq [
        lr!writeData <== lr!contents!val
      , lr!writeEn <== 1
      ]

unbind :: (N m, N n) => LetReplacer n m -> Recipe
unbind lr = lr!state!val!isBound <&> lr!memory!isUnLet |>
        lr!reference!val === lr!memory!contentBits |>
          Seq [
            lr!writeData <== lr!memory!markDelete
          , lr!state <== 3
          ]

isUnbound :: Word N2 -> Bit
isUnbound w = w === 0

isBinding :: Word N2 -> Bit
isBinding w = w === 1

isBound :: Word N2 -> Bit
isBound w = w === 2

incrementAddress :: (N m, N n) => LetReplacer m n -> Recipe
incrementAddress lr =
  lr!address <== lr!address!val + 1