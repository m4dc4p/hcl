module Spec.ReqDefault (tests) where

import Test.HUnit

import System.Console.HCL

tests = TestLabel "reqDefault" $ TestList $ map test'
  [ ( "success", makeReq 1,         2, 1 )
  , ( "failure", reqFail,           2, 2 )
  , ( "error",   Request $ fail "", 2, 2 )
  ]

test' (label, req, def, expect) = TestLabel label $ TestCase $ do
  val <- runRequest $ reqDefault req def
  assertEqual "" (Just expect) val
