module Lists where

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []   = (Nothing, [])
removeAt n list@(x:xs) 
         | n < 0     = (Nothing, list)
         | n == 0    = (Just x, xs)
         | otherwise = (fst a, x : snd a) 
           where a = removeAt (n - 1) xs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = let 
                  merge :: Ord t => [t] -> [t] -> [t]
                  merge a [] = a
                  merge [] b = b
                  merge a@(x:xs) b@(y:ys) 
                        = if (x <= y)
                          then x:(merge xs b)
                          else y:(merge a ys) 
              in merge (mergeSort $ fst m) (mergeSort $ snd m) 
                 where m = splitAt (div (length l) 2) l