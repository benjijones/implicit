{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module Implicit.PolyTree where

import Prelude hiding (foldr, take, drop, splitAt)

data PolyTree a where
  Nil :: PolyTree a
  Cons :: a -> PolyTree a -> PolyTree a
  Many :: a -> PolyTree a -> PolyTree a
  deriving Functor

foldr :: b
        -> (a -> b -> b)
        -> (a -> b -> b)
        -> PolyTree a
        -> b
foldr nil _ _ Nil = nil
foldr nil cons many (Cons head tail) = head `cons` foldr nil cons many tail
foldr nil cons many (Many head tail) = head `many` foldr nil cons many tail

polyZipWith :: (a -> b -> c) -> [a] -> PolyTree b -> PolyTree c
polyZipWith f _ Nil = Nil
polyZipWith f (a:as) (Cons b bs) = Cons (f a b) (polyZipWith f as bs)
polyZipWith f (a:as) (Many b bs) = Many (f a b) (polyZipWith f as bs)

infixr 5 <:+>
(<:+>) :: a -> PolyTree a -> PolyTree a
(<:+>) = Cons

(<:*>) :: Integer -> a -> PolyTree a
0 <:*> a = Nil
n <:*> a = Cons a ((n - 1) <:*> a)