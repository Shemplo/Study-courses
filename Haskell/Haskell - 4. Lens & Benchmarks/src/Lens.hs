{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Lens where

import Control.Applicative
import Data.Functor.Const    as C
import Data.Functor.Identity as I

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a  = Lens s s a a

{- setter funation -}
set :: Lens s t a b -> b -> s -> t
set lns f = I.runIdentity . lns (const $ Identity f)

(.~) :: Lens s t a b -> b -> s -> t
(.~) = set

{- getter funation -}
view :: Lens s t a b -> s -> a
view lns =  getConst . lns Const

(^.) :: Lens s t a b -> s -> a
(^.) = view

{- update funation -}
over :: Lens s t a b -> (a -> b) -> s -> t
over lns f = I.runIdentity . lns (Identity . f)

(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = over

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\ b -> (b, x)) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (\ b -> (x, b)) <$> f a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f obj = fmap (setter obj) (f $ getter obj)

-- Объединить две линзы в одну, которая работает с Either.
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing lns1 lns2 = lens g s where
    g (Left  l) = C.getConst $ lns1 Const l
    g (Right r) = C.getConst $ lns2 Const r

    s (Left  l) b = Left  $ set lns1 b l
    s (Right r) b = Right $ set lns2 b r

-- Изменить цель линзы и вернуть новый результат.
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) lns f obj = (f $ C.getConst $ lns Const obj, over lns f obj)

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) lns f obj = (C.getConst $ lns Const obj, over lns f obj)