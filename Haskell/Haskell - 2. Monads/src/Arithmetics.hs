module Arithmetics where

import Control.Monad

data Expr = Const Int
          | Sum Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          deriving (Show, Eq)

data ArithmeticError = DivideByZero
                     | NegativeExponent
                     deriving (Eq)

instance Show ArithmeticError where
    show DivideByZero     = "Attemp to divide number on 0"
    show NegativeExponent = "Attepmpt to power number in negative exponent"

eval :: Expr -> Either ArithmeticError Int
eval (Const v) = Right v
eval (Sum l r) = liftM2 (+) (eval l) (eval r)
eval (Sub l r) = liftM2 (-) (eval l) (eval r)
eval (Mul l r) = liftM2 (*) (eval l) (eval r)
eval (Div n d) = eval n 
                 >>= \numValue -> eval d 
                 >>= \denomValue -> if denomValue == 0
                                    then Left DivideByZero
                                    else Right $ numValue `div` denomValue
eval (Pow b d) = eval b 
                 >>= \baseValue -> eval d 
                 >>= \degreeValue -> if degreeValue < 0
                                     then Left NegativeExponent
                                     else Right $ baseValue ^ degreeValue
