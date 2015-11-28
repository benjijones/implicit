{-# LANGUAGE GADTs #-}

module Implicit.PolyTree where

import Prelude hiding (foldr, take, drop, splitAt)

data PolyTree a where
  Single :: a -> PolyTree a
  Cons :: a -> PolyTree a -> PolyTree a
  Many :: Integer -> a -> PolyTree a -> PolyTree a

foldr :: (a -> b)
        -> (a -> b -> b)
        -> (Integer -> a -> b -> b)
        -> PolyTree a
        -> b
foldr single _ _ (Single word) = single word
foldr single cons many (Cons head tail) = head `cons` foldr single cons many tail
foldr single cons many (Many n head tail) = many n head $ foldr single cons many tail

infixr 5 +>
(+>) :: a -> PolyTree a -> PolyTree a
(+>) = Cons