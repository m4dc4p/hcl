module Spec.ReqMaybe (tests) where

import Test.HUnit

import System.Console.HCL

tests = TestLabel "reqMaybe" $ TestList $ map test'
  [ ( "success", makeReq 1,         2 )
  , ( "failire", reqFail,           0 )
  , ( "error",   Request $ fail "", 0 )
  ]

test' (label, x, expect) = TestLabel label $ TestCase $ do
  val <- runRequest $ reqMaybe x (makeReq 0) (makeReq . succ)
  assertEqual "" (Just expect) val
