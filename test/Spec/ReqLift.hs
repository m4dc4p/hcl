module Spec.ReqLift (tests) where

import Test.HUnit

import System.Console.HCL

import Spec.Common

tests = TestLabel "reqLift" $ TestList $ map test'
  [ ( Nothing, Nothing )
  , ( Just 1,  Just 2  )
  ]

test' (x, expect) = TestLabel (show x) $ TestCase $ do
  val <- runRequest $ reqLift succ $ req x
  assertEqual "" expect val
