{-# LANGUAGE NamedFieldPuns #-}
module Implicit.Processor where

import Implicit.Atom
import Implicit.EvaluationMemory
import Implicit.LetReplacer as LR
import Implicit.CaseReducer as CR

import Lava.Bit
import Lava.Vector
import Lava.Recipe
import Lava.Generic
import Lava.Ram

data Processor =
  Processor {
    memory :: EvaluationMemory N11 N10,
    address :: Reg N11,
    writeData :: Sig N10,
    writeEn :: Sig N1,

    letReplacer :: LetReplacer N11 N5,
    caseReducer :: CaseReducer N11 N5,

    deleteBit :: Bit,
    cycles :: Reg N4
    --finished :: Sig N1
  }

newProcessor :: [Integer] -> New Processor
newProcessor program = do

  address <- newReg

  writeData <- newSig
  writeEn <- newSig

  let ramInputs = RamInputs {
        ramData = writeData!val,
        ramAddress = address!val,
        ramWrite = writeEn!val!vhead
      }
      memory = evaluationMemory ramInputs program

  letReplacer <- newLetReplacer memory
  caseReducer <- newCaseReducer memory
  cycles <- newReg
  --finished <- newSig

  return $ Processor {
    memory,
    address,
    writeData,
    writeEn,
    letReplacer,
    caseReducer,
    deleteBit = letReplacer!LR.delete!val!vhead
            <|> caseReducer!CR.delete!val!vhead,
    cycles
    --finished
  }

processor :: Integer -> Processor -> Recipe
processor finalAddress p =
  Seq [
    Tick
  , While (p!cycles!val =/= 15) $
    Seq [
      p!letReplacer!letReplace
    , p!caseReducer!caseReduce
    , p!deleteBit |> Seq [
        p!writeData <== p!memory!markDelete
      , p!writeEn <== 1
    ]
    , p!cycles <== p!cycles!val + 1
    , p!address <== p!address!val + 1
    , Tick
    ]
  , p!address <== fromInteger finalAddress
  , Tick
  , Tick
  ]