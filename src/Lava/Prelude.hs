{-# OPTIONS_GHC -XFlexibleInstances #-}

{-|

The beginnings of a Prelude of commonly-used circuits.  By no means
exhaustive, but a useful start.

-}

module Lava.Prelude
  ( -- * Bit-vectors
    Word
  , boolsToWord
    -- * Generalised primitives
  , andG
  , orG
  , delay
  , delayEn
  , (?)
  , nameList
  , nameWord

    -- * Multiplexing
  , select
  , selectG
  , pick
  , pickG

    -- * Encoding and decoding
  , decode
  , decodeTwos
  , encode
  , tally
  , oneHot
  , tal
  , tal'

    -- * Rotation
  , rotr
  , rotateRight
  , rotl
  , rotateLeft
  , dot

    -- * RAMs
  , RamInputs(..)
  , ram
  , dualRam

    -- * Arithmetic
  , Unsigned
  , Signed(..)
  , natSub
  , complement
  , bitPlus
  , wordToInt
  , extend

    -- * Comparators
  , (===)
  , (=/=)
  , Ordered(..)

    -- * Polymorphic functions over lists
  , tree1
  , tree
  , groupN
  , halve
  ) where

import Lava.Bit
import Lava.Vector
import Lava.Word
import Lava.Binary
import Lava.Generic
import Lava.Arithmetic
import Data.List(transpose, inits, tails)

-- | Split a list into sub-lists of maximum length N.
groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = take n xs : groupN n (drop n xs)

-- | N-way multiplexer, with one-hot address.
select :: [Bit] -> [[Bit]] -> [Bit]
select sels inps = map orG
                 $ transpose
                 $ zipWith (\sel -> map (sel <&>)) sels inps



-- | Like 'select', but with zipped arguments.
pick :: [(Bit, [Bit])] -> [Bit]
pick choices = select sels inps
  where (sels, inps) = unzip choices


-- | Binary to one-hot decoder.
decode :: [Bit] -> [Bit]
decode [] = [high]
decode [x] = [inv x, x]
decode (x:xs) = concatMap (\y -> [inv x <&> y, x <&> y]) rest
  where rest = decode xs

-- | Two's complement version of 'decode'.
decodeTwos :: [Bit] -> [Bit]
decodeTwos xs = zipWith (<|>) ys zs
  where (ys, zs) = halve (decode xs)

-- | Split a list in two.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- | One-hot to binary encoder.
encode :: [Bit] -> [Bit]
encode [_] = []
encode as  = zipWith (<|>) (encode ls) (encode rs) ++ [orG rs]
  where (ls,rs) = splitAt (length as `div` 2) as

-- | Binary to tally converter.
tally :: [Bit] -> [Bit]
tally = tal . decode

-- | One-hot to tally converter.
tal :: [Bit] -> [Bit]
tal = map orG . tail . tails

-- | Like 'tal'; specifically @tal\' n  =  tal (n+1)@.
tal' :: [Bit] -> [Bit]
tal' = map orG . init . tails

split :: [a] -> [([a], [a])]
split [] = []
split (x:xs) = ([x], xs) : [(x:y, z) | (y, z) <- split xs]

tac :: ([a], [a]) -> [a]
tac (xs, ys) = reverse xs ++ reverse ys

-- | Dot product over bit-lists.
dot :: [Bit] -> [Bit] -> Bit
dot xs ys = orG (zipWith (<&>) xs ys)

-- | Rotate @b@ by @a@ places to the right; @a@ is a one-hot number.
rotr :: [Bit] -> [Bit] -> [Bit]
rotr a b = map (dot a) (map tac (split b))

-- | Like 'rotr', but lifted to a list of bit-lists.
rotateRight :: [Bit] -> [[Bit]] -> [[Bit]]
rotateRight n = transpose . map (rotr n) . transpose

-- | Like 'rotr', except rotation is to the left.
rotl :: [Bit] -> [Bit] -> [Bit]
rotl (a:as) b = rotr (a:reverse as) b

-- | Like 'rotateRight' except rotation is to the left.
rotateLeft :: [Bit] -> [[Bit]] -> [[Bit]]
rotateLeft n = transpose . map (rotl n) . transpose

-- | Sign-extend a bit-vector.
extend :: N n => Vec (S m) c -> Vec n c
extend n = vextend (vlast n) n

intToOneHot :: Int -> Int -> [Bit]
intToOneHot i w
  | i < 0 = reverse bits
  | otherwise = bits
  where bits = [if abs i == j then high else low | j <- [0..w-1]]

-- | Convert a Haskell @Int@ to a one-hot bit-vector.
oneHot :: N n => Int -> Word n
oneHot i = sized (Vec . intToOneHot i)

------------------------------------- RAMs ------------------------------------

data RamInputs n m =
  RamInputs {
    ramData    :: Word n
  , ramAddress :: Word m
  , ramWrite   :: Bit
  }

-- | RAM of any width and size, with intialiser.
ram :: (N n, N m) => [Integer] -> RamAlgorithm -> RamInputs n m -> Word n
ram init pt inps = Vec $ primRam init pt $
  RamInps {
      dataBus     = velems (vrigid $ ramData inps)
    , addressBus  = velems (vrigid $ ramAddress inps)
    , writeEnable = ramWrite inps
  }

-- | Dual-port RAM of any width and size, with intialiser.
dualRam :: (N n, N m) => [Integer] -> RamAlgorithm
        -> (RamInputs n m, RamInputs n m) -> (Word n, Word n)
dualRam init pt (inps0, inps1) = (Vec out0, Vec out1)
  where
    (out0, out1) =
      primDualRam init pt
        ( RamInps {
            dataBus     = velems (vrigid $ ramData inps0)
          , addressBus  = velems (vrigid $ ramAddress inps0)
          , writeEnable = ramWrite inps0
          }
        , RamInps {
            dataBus     = velems (vrigid $ ramData inps1)
          , addressBus  = velems (vrigid $ ramAddress inps1)
          , writeEnable = ramWrite inps1
          }
        )

---------------------------------- Bit Vectors --------------------------------

instance Generic a => Generic (Vec n a) where
  generic (Vec []) = cons (Vec [])
  generic (Vec (x:xs)) = cons (\x xs -> Vec (x:xs)) >< x >< xs

infix 4 |<=|, |<|, |>|, |>=|

class Ordered a where
  (|<=|) :: a -> a -> Bit
  (|<|)  :: a -> a -> Bit
  (|>=|) :: a -> a -> Bit
  (|>|)  :: a -> a -> Bit

instance Ordered (Vec n Bit) where
  a |<=| b = velems a `ule` velems b
  a |<| b  = velems a `ult` velems b
  a |>=| b = velems a `uge` velems b
  a |>| b  = velems a `ugt` velems b

-- | Subtracts @b@ from @a@, but if @b@ is larger than @a@ then
-- result is @0@.
natSub :: N n => Word n -> Word n -> Word n
natSub a b = Vec $ mapG (last r <&>) (init r)
  where (x, y) = (velems a, velems b)
        r = binAdd high x (map inv y)

------------------------------ Signed Bit Vectors -----------------------------

-- | Signed bit-vectors.
newtype Signed n = Signed (Vec n Bit)
  deriving Show

instance Generic (Signed n) where
  generic (Signed n) = cons Signed >< n

instance Eq (Signed n) where
  a == b = error msg
    where msg = "== and /= on bit-vectors is not supported: try === and =/="

instance N n => Num (Signed n) where
  Signed a + Signed b = Signed (a + b)
  Signed a - Signed b = Signed (a - b)
  a * b = error "(*) on bit-vectors is not yet supported"
  abs (Signed a) = last (velems a) ? (negate (Signed a), Signed a)
  signum (Signed a) = error "signum on bit-vectors is not yet supported"
  fromInteger i = Signed $ sized (\n -> Vec (fromInteger i `ofWidth` n))

instance Ordered (Signed n) where
  Signed a |<=| Signed b = ext1 (velems a) /<=/ ext1 (velems b)
  Signed a |<|  Signed b = ext1 (velems a) /</  ext1 (velems b)
  Signed a |>=| Signed b = ext1 (velems a) />=/ ext1 (velems b)
  Signed a |>|  Signed b = ext1 (velems a) />/  ext1 (velems b)

ext1 :: [Bit] -> [Bit]
ext1 [] = [low]
ext1 xs = xs ++ take 1 (reverse xs)

-- | Returns a list of N named bits with a given prefix.
nameList :: Int -> String -> [Bit]
nameList n s = map (name . (s ++) . show) [1..n]

-- | Returns a vector of N named bits with a given prefix.
nameWord :: N n => String -> Word n
nameWord s = sized (\n -> Vec $ nameList n s)

instance Eq Bit where
  a == b = error "== and /= on bits is not supported."