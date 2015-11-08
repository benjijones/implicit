{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Implicit.AtomType where

import Prelude hiding (Word)

import Lava.Word
import Lava.Vector

data AtomType =
    Data
  | Case
  | Arm
  | Arrow
  | UnCase
  | Let
  | In
  | LetRef
  | LetRefPadding
  | UnLet
  | Add
  deriving Show

encodeAtomType :: (N w) => AtomType -> Word N1 w
encodeAtomType Data = 0
encodeAtomType Case = 1
encodeAtomType Arm = 2
encodeAtomType Arrow = 3
encodeAtomType UnCase = 4
encodeAtomType Let = 5
encodeAtomType In = 6
encodeAtomType LetRef = 7
encodeAtomType LetRefPadding = 8
encodeAtomType UnLet = 9
encodeAtomType Add = 10

decodeAtomType :: (N w) => Word N1 w -> AtomType
decodeAtomType 0 = Data
decodeAtomType 1 = Case
decodeAtomType 2 = Arm
decodeAtomType 3 = Arrow
decodeAtomType 4 = UnCase
decodeAtomType 5 = Let
decodeAtomType 6 = In
decodeAtomType 7 = LetRef
decodeAtomType 8 = LetRefPadding
decodeAtomType 9 = UnLet
decodeAtomType 10 = Add