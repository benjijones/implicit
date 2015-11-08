{-# OPTIONS_GHC -XFlexibleInstances #-}

{-|

The beginnings of a Prelude of commonly-used circuits.  By no means
exhaustive, but a useful start.

-}

module Lava.Prelude
  (-- * Generalised primitives
    andG
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
  , binaryToOneHot
  , decodeTwos
  , oneHotToBinary
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

    -- * Arithmetic
  , Signed(..)
  , natSub
  , complement
  , bitPlus
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

import Prelude hiding (Word)

import Lava.Bit
import Lava.Vector
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
binaryToOneHot :: [Bit] -> [Bit]
binaryToOneHot [] = [high]
binaryToOneHot [x] = [inv x, x]
binaryToOneHot (x:xs) = concatMap (\y -> [inv x <&> y, x <&> y]) rest
  where rest = binaryToOneHot xs

-- | Two's complement version of 'binaryToOneHot'.
decodeTwos :: [Bit] -> [Bit]
decodeTwos xs = zipWith (<|>) ys zs
  where (ys, zs) = halve (binaryToOneHot xs)

-- | Split a list in two.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- | One-hot to binary encoder.
oneHotToBinary :: [Bit] -> [Bit]
oneHotToBinary [_] = []
oneHotToBinary as  = zipWith (<|>) (oneHotToBinary ls) (oneHotToBinary rs) ++ [orG rs]
  where (ls,rs) = splitAt (length as `div` 2) as

-- | Binary to tally converter.
tally :: [Bit] -> [Bit]
tally = tal . binaryToOneHot

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
oneHot :: N n => Int -> Vec n Bit
oneHot i = sized (Vec . intToOneHot i)


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

instance N n => Num (Vec n Bit) where
  a + b = vec (velems a /+/ velems b)
  a - b = vec (velems a /-/ velems b)
  a * b = error "Multiplication of bit-vectors is not yet supported"
  abs a = a
  -- just 0 or 1 as vectors are interpreted as unsigned
  signum a = vec (orG xs : repeat 0)
    where xs = velems a
  fromInteger i = sized (\n -> Vec (i `ofWidth` n))


-- | Subtracts @b@ from @a@, but if @b@ is larger than @a@ then
-- result is @0@.
natSub :: N n => Vec n Bit -> Vec n Bit -> Vec n Bit
natSub a b = Vec $ mapG (last r <&>) (init r)
  where (x, y) = (velems a, velems b)
        r = binAdd high x (map inv y)

ofWidth :: Integral a => a -> Int -> [Bit]
n `ofWidth` s = map boolToBit (intToSizedBin n s)

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
nameWord :: N n => String -> Vec n Bit
nameWord s = sized (\n -> Vec $ nameList n s)

instance Eq Bit where
  a == b = error "== and /= on bits is not supported."