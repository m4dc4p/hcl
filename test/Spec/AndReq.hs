module Spec.AndReq (tests) where

import Test.HUnit

import System.Console.HCL

import Spec.Common

tests = TestLabel "andReq" $ TestList $ map test'
  [ ( Nothing,    Nothing,    Nothing    )
  , ( Just False, Nothing,    Just False )
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
