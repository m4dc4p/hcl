module Spec.OrReq (tests) where

import Test.HUnit

import System.Console.HCL

import Spec.Common

tests = TestLabel "orReq" $ TestList $ map test'
  [ ( Nothing,    Nothing,    Nothing    )
  , ( Just True,  Nothing,    Just True  )
  , ( Just False, Nothing,    Nothing    )
  , ( Nothing,    Just True,  Nothing    )
  , ( Just False, Just False, Just False )
  , ( Just False, Just True,  Just True  )
  , ( Just True,  Just False, Just True  )
  , ( Just True,  Just True,  Just True  )
  ]

test' (x, y, expect) = let label = show x ++ " orReq " ++ show y
  in TestLabel label $ TestCase $ do
    val <- runRequest $ req x `orReq` req y
    assertEqual "" val expect
