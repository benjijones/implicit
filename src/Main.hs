module Main where

import Prelude hiding (splitAt, Word)

--import qualified Implicit.Expr as E
--import Implicit.ExprToAtom
import Implicit.AtomType
import Implicit.Atom
--import Implicit.LetReplacer as LR
--import Implicit.CaseReducer as CR
import Implicit.EvaluationMemory
import Implicit.Let
import Implicit.Word
--import Implicit.Examples
--import Implicit.LetMemory

import Lava.Vector
import Lava.Binary
import Lava.Bit
import Lava.Recipe
import Lava.Binary
import Lava.Vhdl
import Lava.Ram
import Lava.Generic

main :: IO ()
main = do
    mapM_ print . decodeAtoms $ atoms
    mapM_ print . decodeAtom $ letReplace atoms
--    print . unWord $ fst $ splitAt n4 $ atoms
--    print . unWord $ match (A Let +> A Data +> A In +> vempty) atoms (1 :: Word N1 N1) (0 :: Word N1 N1)
--  mapM_ print . zip [1..] . simulateN 12 . (\a -> (select a, output a)) $ newLetMemory letMemoryInput 1

atoms :: Word DataN
atoms = atomsToWord  [ (Atom False Let 0)
                     , (Atom False Data 1)
                     , (Atom False Data 1) ]