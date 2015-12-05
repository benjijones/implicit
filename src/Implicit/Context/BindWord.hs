module Implicit.Context.BindWord where

import Prelude hiding (Word)
import Data.List

import Implicit.Word
import Implicit.Context

bindWord :: b -> Word w -> Context (b, Word w) (Word w)
bindWord b w = Context $ \prevs -> (w, (b, w) : prevs)

getWord :: (Eq b) => b -> Context (b, Word w) (Word w)
getWord b = Context $ \prevs ->
              case find ((b==) . fst) prevs of
                Just (bind, word) -> (word, prevs)
                Nothing -> error "cant find word"