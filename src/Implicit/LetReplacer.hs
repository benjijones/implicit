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
      reference :: Reg (S (S (S (S n)))),
      contents :: Reg (S (S (S (S n)))),
      state :: Reg N2,
      -- 0 : unoccupied, looking for a let
      -- 1 : stored a let binding, now storing the contents
      -- 2 : looking for references
      -- 3 : deleting the let cells?
      address :: Reg m,
      memory :: Word (S (S (S (S n))))
      --writeValue :: Sig (S (S (S (S n))))
    } deriving Show

newLetReplacer :: (N m, N n) => [Integer] -> New (LetReplacer m n)
newLetReplacer program = do
  reference <- newReg
  contents <- newReg
  state <- newReg
  address <- newReg

      --ramInputs :: RamInputs (S (S (S (S n)))) m
  let ramInputs = RamInputs {
        ramData = contents!val,
        ramAddress = address!val,
        ramWrite = writeEn
      }
      memory = EM.evaluationMemory ramInputs program

      writeEn = writeEnable (state!val) (reference!val) memory

      --state = delay 0 (nextState state (reference!val) memory)

  return $ LetReplacer {
    reference,
    contents,
    state,
    address,
    memory
    --writeValue
  }

writeEnable :: Word N2 -> Word n -> Word n -> Bit
writeEnable state reference read =
  (state!isBound) <&> (read === reference)

letReplace :: (N n, N m) => LetReplacer m n -> Recipe
letReplace lr = Seq [
    Tick
  , lr!state!val!isUnbound <&> lr!memory!isLet |>
      lr!state <== 3
  , Tick
  ]

nextState :: (N n) => Word N2 -> Word (S (S (S (S n)))) -> Word (S (S (S (S n)))) -> Word N2
nextState prev reference read =
  if bitToBool (prev!isUnbound) then
    if bitToBool (read!isLet) then 1 else prev
  else if bitToBool (prev!isBinding) then 2
  else if bitToBool (prev!isBound) then
    if bitToBool (read === reference) then 3 else prev
  else prev

isUnbound :: Word N2 -> Bit
isUnbound w = w === 0

isBinding :: Word N2 -> Bit
isBinding w = w === 1

isBound :: Word N2 -> Bit
isBound w = w === 2

{-incrementAddress :: (N m, N n) => LetReplacer m n -> Recipe
incrementAddress lr =
  lr!address <== lr!address!val + 1-}