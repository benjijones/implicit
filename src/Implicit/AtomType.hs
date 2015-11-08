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

instance N w => Encode N1 w AtomType where
  encode Data = 0
  encode Case = 1
  encode Arm = 2
  encode Arrow = 3
  encode UnCase = 4
  encode Let = 5
  encode In = 6
  encode LetRef = 7
  encode LetRefPadding = 8
  encode UnLet = 9
  encode Add = 10

instance (N w) => Decode N1 w AtomType where
  decode 0 = Data
  decode 1 = Case
  decode 2 = Arm
  decode 3 = Arrow
  decode 4 = UnCase
  decode 5 = Let
  decode 6 = In
  decode 7 = LetRef
  decode 8 = LetRefPadding
  decode 9 = UnLet
  decode 10 = Add

instance (N w, Encode (S l1) w AtomType) => Encode l2 w (Vec l2 AtomType) where
  encode = Word . vmap (vhead . unWord . encode)