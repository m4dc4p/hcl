module Spec.NotReq (tests) where

import Test.HUnit

import System.Console.HCL

tests = TestLabel "notReq" $ TestList $ map test'
  [ ( Nothing,    Nothing    )
  , ( Just False, Just True  )
  , ( Just True,  Just False )
  ]

test' (x, expect) = let label = "notReq " ++ show x in
  TestLabel label $ TestCase $ do
    val <- runRequest $ notReq $ reqLiftMaybe x
    assertEqual "" expect val
