{-# LANGUAGE TupleSections #-}
module Implicit.Context where

import Data.List (find)
import Control.Applicative
import Control.Monad

newtype Context b a = Context { runContext :: [b] -> (a, [b]) }

instance (Show a, Show b) => Show (Context b a) where
  show c = show (runContext c [])

instance Functor (Context b) where
  f `fmap` c = Context (\prev -> 
        let (x, binds) = runContext c prev in
            (f x, binds))

instance Monad (Context b) where
  return a = Context (a,)
  oldC >>= f =  Context (\prev ->
      let (x, binds) = runContext oldC prev in
      let (y, newBinds) = runContext (f x) binds in
          (y, newBinds))

instance Applicative (Context b) where
  pure = return
  (<*>) = ap

result :: Context b a -> a
result c = fst (runContext c [])

data Binding b = LetBinding b Integer
               | CaseBinding Integer
               deriving (Eq, Show)

newLetBinding :: b -> Context (Binding b) Integer
newLetBinding bind = Context (\prevs ->
    let newId = nextId . map bindingIndex . filter isLetBinding $ prevs in
    (newId, (LetBinding bind newId):prevs))

getLetBinding :: (Eq b) => b -> Context (Binding b) Integer
getLetBinding b = Context (\prevs ->
  case find ((== b) . getBinder)
    (filter isLetBinding prevs) of
      Just (LetBinding _ x) -> (x, prevs))

newCaseBinding :: Context (Binding b) Integer
newCaseBinding = Context (\prevs ->
    let newId = nextId . map bindingIndex . filter isCaseBinding $ prevs in
    (newId, (CaseBinding newId):prevs))

isLetBinding :: Binding b -> Bool
isLetBinding (LetBinding _ _) = True
isLetBinding _                = False

letBindings :: Context (Binding b) a -> [Binding b]
letBindings c = filter isLetBinding . snd $ runContext c []

getBinder :: Binding b -> b
getBinder (LetBinding b _) = b
getBinder _ = error "only valid on let bindings- use with isLetBinding or letBindings"

isCaseBinding :: Binding b -> Bool
isCaseBinding (CaseBinding _) = True
isCaseBinding _               = False

--caseBindings :: Context b a -> [Binding b]
--caseBindings c = filter isCaseBinding . snd $ runContext c []

bindingIndex :: Binding b -> Integer
bindingIndex (LetBinding _ n) = n
bindingIndex (CaseBinding m)  = m

nextId :: [Integer] -> Integer
nextId prevIds = if null prevIds then 1 else head prevIds + 1