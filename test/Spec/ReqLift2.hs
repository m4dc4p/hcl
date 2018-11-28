module Spec.ReqLift2 (tests) where

import Test.HUnit

import System.Console.HCL

import Spec.Common

tests = TestLabel "reqLift2" $ TestList $ map test'
  [ ( Nothing, Nothing, Nothing )
  , ( Just 1,  Nothing, Nothing )
  , ( Nothing, Just 1,  Nothing )
  , ( Just 1,  Just 1,  Just 2  )
  ]

test' (x, y, expect) = let label = show x ++ " + " ++ show y in
  TestLabel label $ TestCase $ do
    val <- runRequest $ reqLift2 (+) (req x) (req y)
    assertEqual "" expect val
