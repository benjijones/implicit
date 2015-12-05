{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module Implicit.PolyTree where

import Prelude hiding (take, drop, splitAt, zipWith)

import Data.Foldable

data PolyTree a where
  Nil :: PolyTree a
  Cons :: a -> PolyTree a -> PolyTree a
  Many :: a -> PolyTree a -> PolyTree a
  deriving Functor

instance Monoid (PolyTree a) where
  mappend Nil b = b
  mappend (Cons a rest) b = Cons a (mappend rest b)
  mappend (Many a rest) b = Many a (mappend rest b)

  mempty = Nil

instance Applicative PolyTree where
  pure a = Cons a Nil
  (<*>) = zipWith ($)

instance Foldable PolyTree where
  foldr _ nil Nil = nil
  foldr cons nil (Cons head tail) = head `cons` foldr cons nil tail
  foldr cons nil (Many head tail) = head `cons` foldr cons nil tail

instance Traversable PolyTree where
  traverse f Nil = pure Nil
  traverse f (Cons h t) = Cons <$> f h <*> traverse f t
  traverse f (Many h t) = Many <$> f h <*> traverse f t

zipWith :: (a -> b -> c) -> PolyTree a -> PolyTree b -> PolyTree c
zipWith _ Nil Nil = Nil
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)
zipWith f (Many a as) (Many b bs) = Many (f a b) (zipWith f as bs)

polyZipWith :: (a -> b -> c) -> [a] -> PolyTree b -> PolyTree c
polyZipWith f _ Nil = Nil
polyZipWith f (a:as) (Cons b bs) = Cons (f a b) (polyZipWith f as bs)
polyZipWith f (a:as) (Many b bs) = Many (f a b) (polyZipWith f as bs)

getElem :: Integer -> PolyTree a -> a
getElem n Nil = error "index out of bounds"
getElem 0 (Cons a _) = a
getElem n (Cons a rest) = getElem (n-1) rest
getElem 0 (Many a _) = a
getElem n (Many a rest) = getElem (n-1) rest

infixr 5 <:+>
(<:+>) :: a -> PolyTree a -> PolyTree a
(<:+>) = Cons

(<:*>) :: Integer -> a -> PolyTree a
0 <:*> a = Nil
n <:*> a = Cons a ((n - 1) <:*> a)