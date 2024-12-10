module Ast where

data Expr = Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Div Expr Expr
    | Literal Float

eval :: Expr -> Float
eval (Literal x)     = x                            
eval (Plus exprLeft exprRight)  = eval exprLeft + eval exprRight  
eval (Minus exprLeft exprRight) = eval exprLeft - eval exprRight  
eval (Times exprLeft exprRight) = eval exprLeft * eval exprRight  
eval (Div exprLeft exprRight)   = eval exprLeft / eval exprRight 

-- Should eval to "5.0"
test1 = Plus (Literal 3.0) (Literal 2.0)

-- Should eval to "3.5"
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))

-- Should eval to "15.5"
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))

