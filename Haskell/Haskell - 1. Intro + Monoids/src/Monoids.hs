module Monoids where

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat [] = []
maybeConcat list = concat $ map maybe2List list
                   where maybe2List :: (Maybe [a]) -> [a]
                         maybe2List Nothing   = []
                         maybe2List (Just xs) = xs