module Implicit.Examples where

import Implicit.Expr
import Implicit.Context

basicLet :: Expr String
basicLet = Let "x" (Data 1) (LetRef "x")

basicCase :: Expr String
basicCase = Case (Data 1)
                [ (Data 0, Data 2)
                , (Data 1, Data 2)
                ]

caseInLet :: Expr String
caseInLet = Let "x" (Data 1) $
                     Case (Data 0)
                       [ (Data 0, LetRef "x")
                       , (Data 1, Data 2)
                       ]

letInCase :: Expr String
letInCase = Case (Data 1)
              [ (Data 0, Data 1)
              , (Data 1, Let "x" (Data 3) (LetRef "x"))
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