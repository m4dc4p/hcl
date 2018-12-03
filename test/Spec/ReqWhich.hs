module Spec.ReqWhich (tests) where

import Test.HUnit

import System.Console.HCL

tests = TestLabel "reqWhich" $ TestList $ map test'
  [ ( "success", makeReq (),        Right () )
  , ( "failire", reqFail,           Left ()  )
  , ( "error",   Request $ fail "", Left ()  )
  ]

test' (label, x, expect) = TestLabel label $ TestCase $ do
  val <- runRequest $ reqWhich x
  assertEqual "" (Just expect) val
