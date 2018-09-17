{-# LANGUAGE InstanceSigs #-}

module Parser where

import Control.Applicative
import Control.Arrow
import Control.Monad

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser $ \s -> Just (a, s)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    Parser fs <*> Parser sn = Parser mb where
        mb = fs >=> \(f', s) -> sn s 
                >>= \(x, s') -> Just (f' x, s')

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    Parser parser >>= f = Parser mb where
        mb = parser >=> \(ta, s) -> runParser (f ta) s

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser $ const Nothing

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    Parser a <|> Parser b = Parser $ \s -> a s <|> b s

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \s -> case s of
        [] -> Just ((), [])
        _  -> Nothing

satisfy :: (a -> Bool) -> Parser a a
satisfy p = Parser $ \ss -> case ss of
            []     -> Nothing
            (x:xs) -> if p x
                      then Just (x, xs)
                      else Nothing

element :: (a -> Bool) -> Parser a [a]
element p = Parser $ \ss -> _loop ss [] where
            _loop lst@(x:xs) acc = case (runParser (satisfy p) lst) of
                Nothing -> Just (acc, lst)
                _       -> _loop xs (x:acc)
            _loop lst acc = Just (acc, lst)