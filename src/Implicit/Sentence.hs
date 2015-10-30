{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Implicit.Sentence where

import Prelude hiding (Word)

import Lava.Bit
import Lava.Vector
import Lava.Generic

import Lava.Word

type Sentence l a = Vec l a

matches :: Sentence (S l) (Word d w) -> Sentence (S l) (Word d w) -> Bit
pattern `matches` input = vfoldr (<|>) 0 $ vzipWith (===) pattern input