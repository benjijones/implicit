module Main where

import Implicit.Atom
import Implicit.CaseReducer

import Lava.Vector
import Lava.Binary
import Lava.Bit
import Lava.Recipe

main :: IO ()
main = do
    let newCR :: New (CaseReducer N4)
        newCR = newCaseReducer
        result = simRecipe newCR caseReduce (readValue) in
     print result