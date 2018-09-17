module Main where

import Data.List

import Test.Tasty.Hspec
import Test.Tasty
--import Hedgehog

import Arithmetics
import Sequences
import Hierarchy
import Parser

main :: IO ()
main = hspecTestTree >>= \unitTests ->
        let allTests = testGroup "" [unitTests]
        in defaultMain allTests

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "Homework" spec

checkSequence :: (Foldable t, Eq (t a), Eq a, Num a) => Int -> [t a] -> Bool
checkSequence n res = length res == (2 ^ n) 
                        && nub res == res
                        && all (checkLine n) res where 

                        checkLine l xs = length xs == l
                                            && all (`elem` [0, 1]) xs

spec :: Spec
spec = describe "Evaluation" $ do 
        it "constant" $ 
            eval (Const 4) `shouldBe` Right 4
        it "contant 2" $
            eval (Const (-34)) `shouldBe` Right (-34)
        it "a + b" $
            eval (Sum (Const 2) (Const 421)) `shouldBe` Right (421 + 2)
        it "a - b" $
            eval (Sub (Const 4) (Const (-6))) `shouldBe` Right (4 - (-6))
        it "a * b" $
            eval (Mul (Const 3) (Const 32)) `shouldBe` Right (3 * 32)
        it "a / b, b /= 0" $
            eval (Div (Const 432) (Const 4)) `shouldBe` Right (432 `div` 4)
        it "a / b, b == 0" $
            eval (Div (Const 43) (Const 0)) `shouldBe` Left DivideByZero
        it "a ^ b, b > 0" $
            eval (Pow (Const 2) (Const 5)) `shouldBe` Right 32
        it "a ^ b, b < 0" $
            eval (Pow (Const 2) (Const (-4))) `shouldBe` Left NegativeExponent
        it "a * (b + c)" $
            eval (Mul (Const 4) (Sum (Const 6) (Const 30))) `shouldBe` Right (4 * (6 + 30))

        describe "Sequences" $ do
            it "bin 0" $
                bin 0 `shouldBe` [[]]
            it "bin (rand == 5)" $
                let n = 5
                in checkSequence n (bin n) `shouldBe` True

        describe "String summ" $ do
            it "const" $
                stringSum "34" `shouldBe` Just 34
            it "const const" $
                stringSum "43 -32" `shouldBe` Just (43 - 32)
            it "fail" $
                stringSum "fail" `shouldBe` Nothing
            it "const \\n const" $
                stringSum "4 \r\t\n 3" `shouldBe` Just (4 + 3)

        describe "Parser" $ do
            it "ok (empty string)" $
                runParser ok "" `shouldBe` Just ((), "")
            it "ok (string)" $
                runParser ok "value" `shouldBe` Just ((), "value") 
            it "ok (array)" $
                runParser ok [1 :: Int, 2] `shouldBe` Just ((), [1, 2])
            it "eof (empty string)" $
                runParser eof "" `shouldBe` Just ((), "")
            it "eof (string)" $
                runParser eof "value" `shouldBe` Nothing
            it "satisfy (char)" $
                runParser (satisfy (== 0)) [0 :: Int, 1] `shouldBe` Just (0, [1])
            it "satisfy (bad char)" $
                runParser (satisfy (== 0)) [10 :: Int, 1] `shouldBe` Nothing
            it "element (array)" $
                runParser (element (== 0)) [0 :: Int, 0, 1] `shouldBe` Just ([0, 0], [1])
            it "element (full array)" $
                runParser (element (== "a")) ["a", "a"] `shouldBe` Just (["a", "a"], [])