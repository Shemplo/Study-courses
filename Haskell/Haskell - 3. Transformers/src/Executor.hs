module Executor where

import Data.Map.Lazy as Map
import Data.Foldable as Fold

import Control.Exception (throw)
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Cont

import ExecutorIntf
import Parser

execute :: Expr -> Reader (Map String Int) Int
execute (Lit lit) = return lit
execute (Add f s) = liftM2 (+) (execute f) (execute s)
execute (Sub f s) = liftM2 (-) (execute f) (execute s)
execute (Mul f s) = liftM2 (*) (execute f) (execute s)
execute (Div f s) = do
    f' <- execute f
    s' <- execute s
    case s' of
        0 -> throw DivideByZero
        _ -> return (div f' s')
execute (Mod f s) = do
    f' <- execute f
    s' <- execute s
    case s' of
        0 -> throw DivideByZero
        _ -> return (mod f' s')
execute (Var name) = do
    look <- asks (Map.lookup name)
    case look of
        Just v' -> return v'
        Nothing -> throw $ UndefinedVariable name
execute (Let name subst expression) = do
    v <- execute subst
    local (insert name v) (execute expression)

executeStatements :: [Statement] -> ContT RunState (StateT (Map String Int) IO) ()
executeStatements sts = let 
        setContext :: String -> Int -> (StateT (Map String Int) IO) ()
        setContext key value = do
            contains <- gets (member key)
            if contains then
                throw $ ExistingVariable key
            else modify (insert key value)

        updateContext :: String -> Int -> (StateT (Map String Int) IO) ()
        updateContext key value = do
            contains <- gets (member key)
            if contains then
                modify (insert key value)
            else throw $ InvisibleVariable key
        
        removeContext :: String -> (StateT (Map String Int) IO) ()
        removeContext key = do
            contains <- gets (member key)
            if contains then
                modify (delete key)
            else throw $ InvisibleVariable key

        isPositiveState :: String -> Int -> Bool
        isPositiveState cond diff = (cond == "eq" && diff == 0) 
                                    || (cond == "gt" && diff > 0) 
                                    || (cond == "lt" && diff < 0)

        eval :: Statement -> () -> ContT RunState (StateT (Map String Int) IO) ()
        eval st _ = ContT $ \f ->
            case st of
                Define name expr -> do
                    context <- get
                    setContext name $ runReader (execute expr) context
                    res <- f ()
                    removeContext name
                    return res
                Update name expr -> do
                    context <- get
                    updateContext name $ runReader (execute expr) context
                    f ()
                Read name -> do
                    readValue <- lift getLine
                    updateContext name (read readValue :: Int)
                    f ()
                Write expr -> do
                    context <- get
                    (lift . putStrLn) $ show $ runReader (execute expr) context
                    f ()
                Pure _ -> f ()
                For name from to body -> do
                    context <- get
                    setContext name $ runReader (execute from) context
                    let loop = do loopContext <- get
                                  iterator <- gets (Map.! name)
                                  let loopTo = runReader (execute to) loopContext
                                  if iterator >= loopTo then
                                    return Normal
                                  else monad >>= \f' -> case f' of
                                    ExecutorIntf.Break -> return Normal
                                    Normal -> gets (Map.! name) >>= \val ->
                                        updateContext name (val + 1) >> loop
                                  where monad = runContT (executeStatements body) $ const (return Normal)
                    _ <- loop
                    removeContext name
                    f ()
                Parser.Break -> 
                    return ExecutorIntf.Break

                If left cond right body -> do
                    context <- get
                    let sides = zip [0, 1..] $ fmap (\s -> runReader (execute s) context) [left, right]
                    let diff = Fold.foldr (\(a, b) acc -> acc + (1 - 2 * (a `mod` 2)) * b) 0 sides
                    _ <- if isPositiveState cond diff
                         then runContT (executeStatements body) $ const (return Normal)
                         else return Normal
                    f ()

                While left cond right body -> do
                    let loop = do
                            loopContext <- get
                            let sides = zip [0, 1..] $ 
                                        fmap (\s -> runReader (execute s) loopContext) [left, right]
                            let diff = Fold.foldr (\(a, b) acc -> acc + (1 - 2 * (a `mod` 2)) * b) 0 sides
                            if isPositiveState cond diff
                            then monad >>= \f' -> case f' of
                                ExecutorIntf.Break -> return Normal
                                Normal -> loop
                            else return Normal
                            where monad = runContT (executeStatements body) $ const (return Normal)
                    _ <- loop
                    f ()
    in Fold.foldl (>>=) (return ()) $ Prelude.map eval sts
