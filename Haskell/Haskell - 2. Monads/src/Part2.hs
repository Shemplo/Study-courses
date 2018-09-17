{-# LANGUAGE InstanceSigs #-}

module Part2 where

import Control.Arrow
import Control.Monad

newtype Optional a = Optional (Maybe (Maybe a))

instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap f (Optional opt) = Optional $ opt >>= \x -> x >>= \v -> Just $ Just $ f v

instance Applicative Optional where
    pure :: a -> Optional a
    pure = Optional . Just . Just

    (<*>) :: Optional (a -> b) -> Optional a -> Optional b
    Optional Nothing <*> _ = Optional Nothing
    _ <*> Optional Nothing = Optional Nothing
    Optional f <*> v = Optional $ f 
                        >>= \mb -> mb 
                        >>= \mf -> case fmap mf v of
                            Optional Nothing -> Nothing
                            Optional smth    -> smth

instance Monad Optional where
    (>>=) :: Optional a -> (a -> Optional b) -> Optional b
    Optional Nothing >>= _ = Optional Nothing
    Optional mb >>= f = Optional $ mb 
                            >>= \mb' -> mb' 
                            >>= \ma -> case f ma of
                                Optional Nothing -> Nothing
                                Optional smth    -> smth

instance Traversable Optional where
    traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
    traverse _ (Optional Nothing) = Optional Nothing
    traverse f (Optional mb) = undefined