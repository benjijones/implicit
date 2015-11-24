module Implicit.Case where

data CaseReducer l = CaseReducer {
    input :: Word l AtomN
  , contents :: Word l AtomN
  , output :: Word l AtomN
}

caseReduce :: CaseReducer -> CaseReducer