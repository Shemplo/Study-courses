module Sequences where

bin :: Int -> [[Int]]
bin n | n < 0 = error "Negative length of sequences"
bin 0 = [[]]
bin n = bin (n - 1) >>= \x -> [0 : x, 1 : x]