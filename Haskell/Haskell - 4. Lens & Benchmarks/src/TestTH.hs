{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module TestTH where

import TH (ShowAsText (..), deriveShowAsText)

{- Test part of ShowAsText -}
data NormalData = Normal Int Int 
                  deriving Show
                  
data CustomData = FooBar   {foo :: String, bar :: Int} 
                | CustData {value :: Int} 
                  deriving Show

deriveShowAsText ''NormalData
deriveShowAsText ''CustomData

testNormal :: IO ()
testNormal   = print $ TH.show $ Normal 5 6

testFooBar :: IO ()
testFooBar   = print $ TH.show FooBar {foo = "so foo", bar = 4}

testCustData :: IO ()
testCustData = print $ TH.show CustData {value = -1}