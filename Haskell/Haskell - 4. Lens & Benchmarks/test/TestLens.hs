{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Test.Hspec
import Lens
import TH

main :: IO ()
main = hspec $ do
    describe "Template Haskell tests" $ do
        let sampleTuple = ("hello", 10, [4, 3], 2) :: (String, Int, [Int], Int)
        it "First given example" $
            $(chooseByIndices 4 [2, 0]) sampleTuple `shouldBe` 
                (([4, 3], "hello") :: ([Int], String))
        it "Second given example" $
            $(chooseByIndices 4 [1, 1, 3, 1, 1]) sampleTuple `shouldBe` 
                ((10, 10, 2, 10, 10) :: (Int, Int, Int, Int, Int))
    describe "Setter (.~) test" $ do
        it "Test _1. set x = 1 in (x = 0, y = 0)" $
            (_1 .~ 1 $ (0, 0)) `shouldBe` (1, 0)
        it "Test _2. set y = 1 in (x = 0, y = 0)" $
            (_2 .~ 1 $ (0, 0)) `shouldBe` (0, 1)
    describe "Updater (%~) test" $ do
        it "Test _1. (+1) operation in (x = 0, y = 0)" $
            (_1 %~ (+ 1) $ (0, 0)) `shouldBe` (1, 0)
        it "Test _2. (*3) operation in (x = 2, y = 2)" $
            (_2 %~ (* 3) $ (2, 2)) `shouldBe` (2, 2 * 3)
    describe "Getter (^.) test" $ do
        it "Test _1. Get x from (x = -1, y = 2)" $
            (_1 ^. (-1, 2)) `shouldBe` (-1)
        it "Test _1. Get y from (x = -1, y = 2)" $
            (_2 ^. (-1, 2)) `shouldBe` 2
    describe "Combination of operations" $
        it "(x = 2, y = 3) -> (x - y, x)" $
            pair' `shouldBe` (-1, 2) where
                pair = (2, 3)

                pair' = _2 .~ (_1 ^. pair) $ _1 %~ (-(_2 ^. pair) +) $ pair