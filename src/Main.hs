module Main where

import Prelude hiding (splitAt, Word)

--import qualified Implicit.Expr as E
--import Implicit.ExprToAtom
import Implicit.AtomType
import Implicit.Atom
import Implicit.Match
--import Implicit.LetReplacer as LR
--import Implicit.CaseReducer as CR
import Implicit.EvaluationMemory
import Implicit.Let
--import Implicit.Examples
--import Implicit.LetMemory

import Lava.Vector
import Lava.Binary
import Lava.Bit
import Lava.Recipe
import Lava.Binary
import Lava.Vhdl
import Lava.Ram
import Lava.Word
import Lava.Generic

main :: IO ()
main = do
    putStrLn . showAtoms . decodeAtoms  $ atoms
--    putStrLn . showAtoms . decodeAtoms $ output letReplacer
--    print . unWord $ fst $ splitAt n4 $ atoms
--    print . unWord $ match (A Let +> A Data +> A In +> vempty) (fst $ splitAt n4 $ atoms) (1 :: Word N1 N1) (0 :: Word N1 N1)
--  mapM_ print . zip [1..] . simulateN 12 . (\a -> (select a, output a)) $ newLetMemory letMemoryInput 1

atoms :: Word N3 AtomN
atoms = encodeVector encodeAtom $ (Atom False Let 0) +> (Atom False Data 0) +> (Atom False In 0) +> vempty

letReplacer = letReplace atoms (LetReplacer 0 0)