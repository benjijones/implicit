module Lava.Generic where

import Lava.Bit
import Lava.Vector

-- | Parallel reduce for a commutative an associative operator.  Input
-- list must be non-empty.
tree1 :: (a -> a -> a) -> [a] -> a
tree1 f [x] = x
tree1 f (x:y:ys) = tree1 f (ys ++ [f x y])

-- | Like 'tree1', but input list may be empty, in which case the zero
-- element is returned.
tree :: (a -> a -> a) -> a -> [a] -> a
tree f z xs = if null xs then z else tree1 f xs


instance Generic a => Generic (Vec n a) where
  generic (Vec []) = cons (Vec [])
  generic (Vec (x:xs)) = cons (\x xs -> Vec (x:xs)) >< x >< xs

-- | Logical AND of all bits in a structure.
andG :: Generic a => a -> Bit
andG = tree (<&>) high . bits

-- | Logical OR of all bits in a structure.
orG :: Generic a => a -> Bit
orG = tree (<|>) low . bits

infix 4 ===
-- | Generic equality.
(===) :: Generic a => a -> a -> Bit
a === b = andG $ bits $ zipWithG (<=>) a b

infix 4 =/=
-- | Generic diseqaulity.
(=/=) :: Generic a => a -> a -> Bit
a =/= b = inv $ andG $ bits $ zipWithG (<=>) a b

-- | Generic register, with initialiser.
delay :: Generic a => a -> a -> a
delay init a = lazyZipWithG delayBit init a

-- | Generic register, with initialiser, with input-enable.
delayEn :: Generic a => a -> Bit -> a -> a
delayEn init en a = lazyZipWithG (delayBitEn en) init a

-- | Generic two-way multiplexer.
(?) :: Generic a => Bit -> (a, a) -> a
cond ? (a, b) = zipWithG (muxBit cond) b a

-- | Compare 2 inputs using (===)
-- return 'true' param if they match,
-- 'false' param otherwise
match :: (Generic a, Generic b) => a -> a -> b -> b -> b
match a input true false = ((input === a) ? (true, false))

-- | Generic 'select'.
selectG :: Generic a => [Bit] -> [a] -> a
selectG sels inps = tree1 (zipWithG (<|>))
                  $ zipWith (\sel -> mapG (sel <&>)) sels inps

-- | Generic 'pick'.
pickG :: Generic a => [(Bit, a)] -> a
pickG choices = selectG sels inps
  where (sels, inps) = unzip choices
