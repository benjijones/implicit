module Implicit.Adder where

import Prelude hiding (Word, splitAt)

import Implicit.AtomType
import Implicit.Atom
import Implicit.Word
import Implicit.PolyTree
import Implicit.Context.BindWord
import Implicit.ShiftRegister

import Lava.Vector
             
adder :: Word AtomN -> Word AtomN
adder = getElem 2 . match ((A Add, return) <:+>
                           (A Data, bindWord "arg1") <:+>
                           (A Data, \arg2 -> do 
                                arg1 <- getWord "arg1"
                                return (mapContents ((+) . contents $ arg1) arg2)) <:+> Nil) . serialToParallel 3