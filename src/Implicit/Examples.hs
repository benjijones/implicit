module Implicit.Examples where

import Implicit.Expr
import Implicit.Context

exampleLet :: Expr String
exampleLet = Let "x" (Data 1) (LetRef "x")

exampleCase :: Expr String
exampleCase = Case (Data 1)
                [ (Data 0, Data 2)
                , (Data 1, Data 2)
                ]

exampleContext :: Context String Integer
exampleContext = do
    x <- newLetBinding "x"
    y <- newLetBinding "y"
    z <- newCaseBinding
    z1 <- newCaseBinding
    w <- newLetBinding "x"
    x1 <- getLetBinding "x"
    return (x1)