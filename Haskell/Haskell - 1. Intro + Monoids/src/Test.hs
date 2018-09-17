{-# LANGUAGE RankNTypes #-}

module Test where

import Data.List.NonEmpty

import Intro
import Lists
import Civil
import Natural
import Monoids
import Split

makeTest :: Eq a => (a -> a) -> a -> a -> Bool
makeTest func f t = runTest f t func

runTest :: forall a.Eq a => a -> a -> (a -> a) -> Bool
runTest a b f = b == f a

checkResult :: (Int, Bool) -> Bool
checkResult (i, False) = error ("Test " ++ (show i) ++ " failed")
checkResult _          = True

{- BLOCK 1 // Intro -}

testOrder3 :: Bool
testOrder3 = checkResult (1, makeTest (order3) (1 :: Int, 1, 1) (1, 1, 1)) &&
             checkResult (2, makeTest (order3) (4 :: Int, 1, 1) (1, 1, 4)) &&
             checkResult (3, makeTest (order3) (5 :: Int, 2, 1) (1, 2, 5)) &&
             checkResult (4, makeTest (order3) (32.2 :: Double, 40, 0.4) (0.4, 32.2, 40)) &&
             checkResult (5, makeTest (order3) ("c", "b", "a") ("a", "b", "c")) &&
             checkResult (6, makeTest (order3) ("ba", "aa", "ac") ("aa", "ac", "ba"))

testSmartR :: Bool
testSmartR = checkResult (1, makeTest (smartReplicate) [] []) &&
             checkResult (2, makeTest (smartReplicate) [0] []) &&
             checkResult (3, makeTest (smartReplicate) [1] [1]) &&
             checkResult (4, makeTest (smartReplicate) [2] [2, 2]) &&
             checkResult (5, makeTest (smartReplicate) [1, 2] [1, 2, 2]) &&
             checkResult (6, makeTest (smartReplicate) [2, 2] [2, 2, 2, 2]) &&
             checkResult (7, makeTest (smartReplicate) [1, 2, 2, 3] [1, 2, 2, 2, 2, 3, 3, 3]) &&
             checkResult (8, makeTest (smartReplicate) [1, 2, 3, 2] [1, 2, 2, 3, 3, 3, 2, 2]) &&
             checkResult (9, makeTest (smartReplicate) [100] (replicate 100 100))
             
testContains :: Bool
testContains = checkResult (1, makeTest (contains (0 :: Int)) [] []) &&
               checkResult (2, makeTest (contains (0 :: Int)) [[0]] [[0]]) &&
               checkResult (3, makeTest (contains (0 :: Int)) [[0], [0, 1]] [[0], [0, 1]]) &&
               checkResult (4, makeTest (contains (0 :: Int)) [[1]] []) &&
               checkResult (5, makeTest (contains (0 :: Int)) [[1, 1], [1, 0]] [[1, 0]]) &&
               checkResult (8, makeTest (contains (3 :: Int)) [[1..5], [2,0], [3,4]] [[1,2,3,4,5],[3,4]]) &&
               checkResult (9, makeTest (contains (0 :: Double)) [[0.0, 0.1]] [[0.0, 0.1]]) &&
               checkResult (10, makeTest (contains (1 :: Double)) [[1, 2, 3], [0, 2, 3]] [[1, 2, 3]]) &&
               checkResult (11, makeTest (contains (1.3 :: Double)) [[0], [2, 1.5]] []) &&
               checkResult (12, makeTest (contains (1.4 :: Double)) [] []) &&
               checkResult (13, makeTest (contains "a") 
                                [["b", "c"], ["aa", "b"], ["d", "a"], ["a", "b", "c", "d"]] 
                                [["d", "a"], ["a", "b", "c", "d"]])
               
testStringS :: Bool
testStringS = checkResult (1, (stringSum "") == 0) &&
              checkResult (2, (stringSum "1 1") == 2) &&
              checkResult (3, (stringSum "1 2 3") == 6) &&
              checkResult (4, (stringSum "1 ") == 1) &&
              checkResult (5, (stringSum "\t1\t") == 1) &&
              checkResult (6, (stringSum "\t12345\t") == 12345) &&
              checkResult (7, (stringSum "010 020 030") == 60) &&
              checkResult (8, (stringSum "-1 -2 -3") == -6) &&
              checkResult (9, (stringSum "\t-12345\t") == -12345) &&
              checkResult (10, (stringSum "\n1\t\n3   555  -1\n\n\n-5") == 553) &&
              checkResult (11, (stringSum "123\t\n\t\n\t\n321 -4 -40") == 400)
              
{- BLOCK 2 // Lists -}

testRemoveAt :: Bool
testRemoveAt = checkResult (1, (removeAt 0 [1 :: Int]) == (Just 1, [])) &&
               checkResult (2, (removeAt 0 [1 :: Int, 2]) == (Just 1, [2])) &&
               checkResult (3, (removeAt 0 [1 :: Int, 2, 3]) == (Just 1, [2, 3])) &&
               checkResult (4, (removeAt 1 [1 :: Int, 2, 3]) == (Just 2, [1, 3])) &&
               checkResult (5, (removeAt 5 [1 :: Int, 2, 3]) == (Nothing, [1, 2, 3])) &&
               checkResult (6, (removeAt 0 ["a", "b", "c"]) == (Just "a", ["b", "c"])) &&
               checkResult (7, (removeAt 2 [1 :: Int, 2, 3]) == (Just 3, [1, 2])) &&
               checkResult (8, (removeAt (-2) [1 :: Int, 2, 3]) == (Nothing, [1, 2, 3]))
               
testSort :: Bool
testSort = checkResult (1, makeTest (mergeSort) [1 :: Int] [1]) &&
           checkResult (2, makeTest (mergeSort) [1 :: Int, 2] [1, 2]) &&
           checkResult (3, makeTest (mergeSort) [2 :: Int, 1] [1, 2]) &&
           checkResult (4, makeTest (mergeSort) [1 :: Int, 3, 2] [1, 2, 3]) &&
           checkResult (5, makeTest (mergeSort) [10 :: Int, 9, 8, 7, 6, 5] [5 .. 10]) &&
           checkResult (6, makeTest (mergeSort) ["b", "c", "a"] ["a", "b", "c"]) &&
           checkResult (7, makeTest (mergeSort) [100 :: Int, 99 .. 1] [1 .. 100])
           
{- BLOCK 3 // Civil -}

testNextD :: Bool
testNextD = checkResult (1, makeTest (nextDay) Mon Tue) &&
            checkResult (2, makeTest (nextDay) Tue Wed) &&
            checkResult (3, makeTest (nextDay) Wed Thu) &&
            checkResult (4, makeTest (nextDay) Thu Fri) &&
            checkResult (5, makeTest (nextDay) Fri Sat) &&
            checkResult (6, makeTest (nextDay) Sat Sun) &&
            checkResult (7, makeTest (nextDay) Sun Mon)
            
testAfterD :: Bool
testAfterD = checkResult (1, makeTest (afterDays 2) Mon Wed) &&
             checkResult (2, makeTest (afterDays 2) Sun Tue) &&
             checkResult (3, makeTest (afterDays 2) Thu Sat) &&
             checkResult (4, makeTest (afterDays 3) Mon Thu) &&
             checkResult (5, makeTest (afterDays 3) Sun Wed) &&
             checkResult (6, makeTest (afterDays 8) Mon Tue) &&
             checkResult (7, makeTest (afterDays 10) Fri Mon) &&
             checkResult (8, makeTest (afterDays 11) Sat Wed)
             
testWEnd :: Bool
testWEnd = checkResult (1, (isWeekend Mon) == False) &&
           checkResult (2, (isWeekend Tue) == False) &&
           checkResult (3, (isWeekend Wed) == False) &&
           checkResult (4, (isWeekend Thu) == False) &&
           checkResult (5, (isWeekend Fri) == False) &&
           checkResult (6, (isWeekend Sat)) &&
           checkResult (7, (isWeekend Sun))
           
testPartyD :: Bool
testPartyD = checkResult (1, (daysToParty Fri) == 0) &&
             checkResult (2, (daysToParty Thu) == 1) &&
             checkResult (3, (daysToParty Wed) == 2) &&
             checkResult (4, (daysToParty Tue) == 3) &&
             checkResult (5, (daysToParty Mon) == 4) &&
             checkResult (6, (daysToParty Sun) == 5) &&
             checkResult (7, (daysToParty Sat) == 6)
             
{- BLOCK 3 // Natural -}

testNatEq :: Bool
testNatEq = checkResult (1, Z == Z) &&
            checkResult (2, S Z == S Z) &&
            checkResult (3, S (S Z) == S (S Z)) &&
            checkResult (4, S (S Z) /= S Z) &&
            checkResult (5, Z /= S (S Z)) &&
            checkResult (6, (12 :: Nat) >= (2 :: Nat)) &&
            checkResult (7, (12 :: Nat) * Z == Z)
            
testNatOp :: Bool
testNatOp = checkResult (1, Z + Z == Z) &&
            checkResult (2, Z + (S Z) == S Z) &&
            checkResult (3, (S Z) + (S Z) == S (S Z)) &&
            checkResult (4, Z * Z == Z) &&
            checkResult (5, Z * (S Z) == Z) &&
            checkResult (6, (S Z) * (S Z) == S Z) &&
            checkResult (7, (S Z) + (S (S Z)) == S (S (S Z))) &&
            checkResult (8, (S (S Z)) + (S (S Z)) == S (S (S (S Z)))) &&
            checkResult (9, Z - Z == Z) &&
            checkResult (10, (S Z) - Z == S Z) &&
            checkResult (11, (S Z) - (S Z) == Z) &&
            checkResult (12, (S (S Z)) - (S Z) == S Z) &&
            checkResult (13, S (S (S Z)) * (S (S (S Z))) - (S Z) == (fromInteger 8)) &&
            checkResult (14, abs (S Z) == S Z) &&
            checkResult (15, signum Z == 0)
            
-- City Nothing Nothing (Two :| [One, One])
-- buildCastle (City Nothing Nothing (Two :| [One, One]))
-- inviteLord (snd (buildCastle (City Nothing Nothing (Two :| [One, One])))) Lord
-- buildHouse (City Nothing Nothing (Two :| [One, One])) 3
-- cityPopulation (buildHouse (City Nothing Nothing (Two :| [One, One])) 3)
-- buildHouse (buildHouse (City Nothing Nothing (Two :| [One, One])) 3) 4
-- inviteLord (snd (buildCastle (buildHouse (buildHouse (City Nothing Nothing (Two :| [One, One])) 3) 4))) Lord
-- buildWalls (inviteLord (snd (buildCastle (buildHouse (buildHouse (City Nothing Nothing (Two :| [One, One])) 3) 4))) Lord)

{- BLOCK 5 // Monoids -}

testMaybeC :: Bool
testMaybeC = checkResult (1, (maybeConcat [Just [1 :: Int]]) == [1]) &&
             checkResult (2, (maybeConcat [Just [1 :: Int], Nothing]) == [1]) &&
             checkResult (3, (maybeConcat [Just [1 :: Int], Just [2 :: Int]]) == [1, 2]) &&
             checkResult (4, (maybeConcat [Just [1 :: Int], Nothing, Just [3 :: Int]]) == [1, 3]) &&
             checkResult (5, (maybeConcat 
                                [Just [1 :: Int, 2, 3], Nothing, Just [4 :: Int, 5]]) == [1 .. 5])
                                
testSplitOn :: Bool
testSplitOn = checkResult (1, (splitOn '/' "path")         == ("path" :| [])) &&
              checkResult (2, (splitOn '/' "path to path") == ("path to path" :| [])) &&
              checkResult (3, (splitOn '/' "path/next")    == ("path" :| ["next"])) &&
              checkResult (4, (splitOn '/' "path/a/b")     == ("path" :| ["a", "b"])) &&
              checkResult (5, (splitOn '/' "a/b c/d")      == ("a" :| ["b c", "d"]))
               