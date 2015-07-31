module Lava.Arithmetic where

import Lava.Bit

instance Num Bit where
  a + b = a <#> b
  a - b = a <&> inv b
  a * b = a <&> b
  abs a = a
  signum a = a
  fromInteger i = if i == 0 then low else high

fullAdd :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdd cin a b = (sum, cout)
  where sum' = a <#> b
        sum  = xorcy (sum', cin)
        cout = muxcy sum' (a, cin)

binAdd :: Bit -> [Bit] -> [Bit] -> [Bit]
binAdd c a b = add c a b
  where
    add c [a]    [b]    = [sum, cout]
      where (sum, cout) = fullAdd c a b
    add c (a:as) [b]    = add c (a:as) [b,b]
    add c [a]    (b:bs) = add c [a,a] (b:bs)
    add c (a:as) (b:bs) = sum : add cout as bs
      where (sum, cout) = fullAdd c a b

infixl 6 /+/
(/+/) :: [Bit] -> [Bit] -> [Bit]
a /+/ b = init (binAdd low a b)

infixl 6 /-/
(/-/) :: [Bit] -> [Bit] -> [Bit]
a /-/ b = init (binAdd high a (map inv b))

infix 4 /</
(/</) :: [Bit] -> [Bit] -> Bit
a /</ b = last (a /-/ b)

infix 4 /<=/
(/<=/) :: [Bit] -> [Bit] -> Bit
a /<=/ b = inv (b /</ a)

infix 4 />/
(/>/) :: [Bit] -> [Bit] -> Bit
a />/ b = b /</ a

infix 4 />=/
(/>=/) :: [Bit] -> [Bit] -> Bit
a />=/ b = b /<=/ a

ult :: [Bit] -> [Bit] -> Bit
a `ult` b = inv $ last $ binAdd high a (map inv b)

ule :: [Bit] -> [Bit] -> Bit
a `ule` b = inv (b `ult` a)

ugt :: [Bit] -> [Bit] -> Bit
a `ugt` b = b `ult` a

uge :: [Bit] -> [Bit] -> Bit
a `uge` b = b `ule` a

-- | Two's complement of a bit-list.
complement :: [Bit] -> [Bit]
complement a = init $ binAdd high (map inv a) [low]

-- | Addition of a single bit to a bit-list.
bitPlus :: Bit -> [Bit] -> [Bit]
bitPlus a b = init (binAdd a (map (const low) b) b)