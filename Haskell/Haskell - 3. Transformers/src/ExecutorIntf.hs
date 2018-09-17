module ExecutorIntf where

import Control.Exception (Exception)

data EvaluateException = DivideByZero
    | UndefinedVariable String
    | InvisibleVariable String
    | ExistingVariable String
    deriving Show

instance Exception EvaluateException

data Expr = Lit Int
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Let String Expr Expr
    deriving Show

data RunState = Normal | Break