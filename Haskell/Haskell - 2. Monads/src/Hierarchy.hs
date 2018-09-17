{-# LANGUAGE InstanceSigs #-}

module Hierarchy where

import Text.Read

stringSum :: String -> Maybe Int
stringSum = fmap sum . traverse readMaybe . words

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

{-
    1. Left Identity: 
    return a >>= f ≡ f a

        return a >>= f 
            ≡ Optional (Just (Just a)) >>= f (1)
            ≡ f a (2)

    2. Right Identity: 
    m >>= return ≡ m

        2.1 m ≡ Optional Nothing:
        Optional Nothing >>= return
            ≡ Optional Nothing (4

        2.2 m ≡ Optional (Just Nothing):
        Optional (Just Nothing) >>=s return
            ≡ Optional (Just Nothing) (3)

        2.3 m ≡ Optional (Just (Just a)):
        Optional (Just (Just a)) >>= return
            ≡ return a (2)
            ≡ Optional (Just (Just a)) (1)

    3. Associativity: 
    (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

        3.1 m ≡ Optional Nothing:
        (Optional Nothing >>= f) >>= g
            ≡ (Optional Nothing) >>= g (4)
            ≡ Optional Nothing (4)
        (Optional Nothing) >>= (\x -> f x >>= g)
            ≡ Optional Nothing (4)

        3.2 m ≡ Optional (Just Nothing):
        (Optional (Just Nothing) >>= f) >>= g
            ≡ (Optional (Just Nothing)) >>= g (3)
            ≡ Optional (Just Nothing) (3)
        (Optional (Just Nothing)) >>= (\x -> f x >>= g)
            ≡ Optional (Just Nothing) (3)

        3.3 m ≡ Optional (Just (Just a)):
        (Optional (Just (Just a)) >>= f) >>= g
            ≡ (f a) >>= g (2)
        Optional (Just (Just a)) >>= (\x -> f x >>= g)
            ≡ (\x -> f x >>= g) a (2)
            ≡ f a >>= g (function app)
            
-}

instance Monad Optional where
    (>>=) :: Optional a -> (a -> Optional b) -> Optional b
    Optional Nothing >>= _ = Optional Nothing
    Optional mb >>= f = Optional $ mb 
                            >>= \mb' -> mb' 
                            >>= \ma -> case f ma of
                                Optional Nothing -> Nothing
                                Optional smth    -> smth