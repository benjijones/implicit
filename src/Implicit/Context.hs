{-# LANGUAGE TupleSections #-}
module Implicit.Context where

import Data.List (find)

newtype Context b a = Context { runContext :: [Binding b] -> (a, [Binding b]) }

instance (Show a, Show b) => Show (Context b a) where
  show c = show (runContext c [])

instance Monad (Context b) where
  return a = Context (a,)
  oldC >>= f =  Context (\prev ->
      let (x, binds) = runContext oldC prev in
      let (y, newBinds) = runContext (f x) binds in
          (y, newBinds))

result :: Context b a -> a
result c = fst (runContext c [])

data Binding b = LetBinding b Integer
               | CaseBinding Integer
               deriving (Eq, Show)

newLetBinding :: b -> Context b Integer
newLetBinding bind = Context (\prevs ->
    let newId = nextId . map bindingIndex . filter isLetBinding $ prevs in
    (newId, (LetBinding bind newId):prevs))

getLetBinding :: (Eq b) => b -> Context b Integer
getLetBinding b = Context (\prevs ->
  case find ((== b) . getBinder)
    (filter isLetBinding prevs) of
      Just (LetBinding _ x) -> (x, prevs))

newCaseBinding :: Context b Integer
newCaseBinding = Context (\prevs ->
    let newId = nextId . map bindingIndex . filter isCaseBinding $ prevs in
    (newId, (CaseBinding newId):prevs))

isLetBinding :: Binding b -> Bool
isLetBinding (LetBinding _ _) = True
isLetBinding _                = False

letBindings :: Context b a -> [Binding b]
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
nextId prevIds = if null prevIds then 0 else head prevIds + 1