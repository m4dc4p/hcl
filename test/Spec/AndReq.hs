module Spec.AndReq (tests) where

import Test.HUnit

import System.Console.HCL

tests = TestLabel "andReq" $ TestList $ map test'
  [ ( Nothing,    Nothing,    Nothing    )
  , ( Just True,  Nothing,    Nothing    )
  , ( Nothing,    Just True,  Nothing    )
  , ( Just False, Just False, Just False )
  , ( Just False, Just True,  Just False )
  , ( Just True,  Just False, Just False )
  , ( Just True,  Just True,  Just True  )
  ]

test' (x, y, expect) = let label = show x ++ " andReq " ++ show y
  in TestLabel label $ TestCase $ do
    val <- runRequest $ req x `andReq` req y
    assertEqual "" val expect

req x = case x of
  Just x  -> makeReq x
  Nothing -> reqFail
