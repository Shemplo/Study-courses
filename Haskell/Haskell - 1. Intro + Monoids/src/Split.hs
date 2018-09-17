module Split where

import Data.List.NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn x = foldr folder ([] :| [])
            where folder y res@(first :| rest)
                         | (y /= x) = (y : first) :| rest
                         | otherwise = [] <| res

joinWith :: a -> NonEmpty [a] -> [a]
joinWith x = foldr1 folder
             where folder y res = y ++ x : res