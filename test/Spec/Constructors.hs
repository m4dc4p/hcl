module Spec.Constructors (tests) where

import Test.HUnit

import System.Console.HCL

tests = TestLabel "constructors" $ TestList [makeReqTests, reqFailTest]

makeReqTests = TestLabel "makeReq" $ TestList $ map test' [ True, False ]

test' x = TestLabel (show x) $ TestCase $ do
  val <- runRequest $ makeReq x
  assertEqual "" (Just x) val

reqFailTest = TestLabel "reqFail" $ TestCase $ do
  val <- runRequest (reqFail :: Request ())
  assertEqual "" Nothing val
