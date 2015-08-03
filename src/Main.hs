module Main where

import Implicit.Atom
import Implicit.Processor

import Lava.Vector
import Lava.Binary
import Lava.Bit
import Lava.Recipe

main :: IO ()
main = do
    let newCR :: New (Processor)
        newCR = newProcessor
        result = simRecipe newCR processor (finished) in
     print result