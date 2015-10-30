{-# LANGUAGE NamedFieldPuns #-}
module Implicit.LetReplacer where

import Prelude hiding (Word)

import Implicit.Atom
import qualified Implicit.EvaluationMemory as EM
--import Implicit.LetMemory

import Lava.Word
import Lava.Vector
import Lava.Recipe
import Lava.Generic
import Lava.Bit
import Lava.Recipe

data LetReplacer addrN dataN =
    LetReplacer {
      reference :: Reg dataN,
      contents :: Reg (S (S (S (S (S dataN))))),
      state :: Reg N2,
      -- 0 : store let binder and delete
      -- 1 : store let contents and delete
      --     delete trailing In
      -- 2 : replace LetRef and delete
      -- 3 : finished
      delete :: Sig N1,
      replace :: Sig N1,
      replaceWith :: Sig (S (S (S (S (S dataN))))),
      memoryIn :: Word (S (S (S (S (S dataN)))))
    } deriving Show

newLetReplacer :: (N addrN, N dataN) => Word (S (S (S (S (S dataN))))) -> New (LetReplacer addrN dataN)
newLetReplacer memoryIn = do
  reference <- newReg
  contents <- newReg
  state <- newReg

  delete <- newSig
  replace <- newSig
  replaceWith <- newSigDef (val contents)

  return $ LetReplacer {
    reference,
    contents,
    state,
    delete,
    replace,
    replaceWith,
    memoryIn
  }

letReplace :: (N m, N n) => LetReplacer m n -> Recipe
letReplace lr = Seq [
    lr!bindLetReference
  , lr!bindLetContents
  , lr!replaceLet
  , lr!unbindLet
  ]

bindLetReference :: (N m, N n) => LetReplacer m n -> Recipe
bindLetReference lr = lr!isUnbound <&> lr!memoryIn!isLet |>
      Seq [
        lr!reference <== lr!memoryIn!contentBits
      , lr!delete <== 1
      , lr!state <== 1
      ]

bindLetContents :: (N m, N n) => LetReplacer n m -> Recipe
bindLetContents lr = lr!isBinding |>
  Seq [
    lr!memoryIn!isIn!inv |> Seq [
      lr!contents <== lr!memoryIn
    , lr!delete <== 1
    ]
  , lr!memoryIn!isIn |> Seq [
      lr!delete <== 1
    , lr!state <== 2
    ]
  ]


replaceLet :: (N m, N n) => LetReplacer n m -> Recipe
replaceLet lr = lr!isBound <&> lr!memoryIn!isLetRef |>
  lr!replace <== 1

unbindLet :: (N m, N n) => LetReplacer n m -> Recipe
unbindLet lr = lr!isBound <&> lr!memoryIn!isUnLet |>
  lr!reference!val === lr!memoryIn!contentBits |>
    Seq [
      lr!delete <== 1
    , lr!state <== 3
    ]

isUnbound :: LetReplacer m n -> Bit
isUnbound = (=== 0) . val . state

isBinding :: LetReplacer m n -> Bit
isBinding = (=== 1) . val . state

isBound :: LetReplacer m n -> Bit
isBound = (=== 2) . val . state