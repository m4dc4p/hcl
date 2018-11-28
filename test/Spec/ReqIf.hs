module Spec.ReqIf (tests) where

import Test.HUnit

import System.Console.HCL

import Spec.Common

tests = TestLabel "reqIf" $ TestList $ map test'
  [ ( Nothing,    Nothing  )
  , ( Just True,  Just 'a' )
  , ( Just False, Just 'b' )
  ]

test' (x, expect) = let label = "reqIf " ++ show x in
  TestLabel label $ TestCase $ do
    val <- runRequest $ reqIf (req x) (return 'a') (return 'b')
    assertEqual "" expect val
