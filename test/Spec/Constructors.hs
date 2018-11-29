module Spec.Constructors (tests) where

import Test.HUnit

import System.Console.HCL

tests = TestLabel "constructors" $ TestList
  [ makeReqTests
  , reqFailTest
  , reqLiftMaybeTests
  , reqIOTests
  ]

makeReqTests = TestLabel "makeReq" $ TestList $ map makeReqTest [ True, False ]

makeReqTest x = TestLabel (show x) $ TestCase $ do
  val <- runRequest $ makeReq x
  assertEqual "" (Just x) val

reqFailTest = TestLabel "reqFail" $ TestCase $ do
  val <- runRequest (reqFail :: Request ())
  assertEqual "" Nothing val

reqLiftMaybeTests = TestLabel "reqMaybe" $ TestList $ map reqLiftMaybeTest
  [Nothing, Just ()]

reqLiftMaybeTest x = TestLabel (show x) $ TestCase $ do
  val <- runRequest $ reqLiftMaybe x
  assertEqual "" x val

reqIOTests = TestLabel "reqIO" $ TestList $ map reqIOTest
  [ ( "success", return (), Just () )
  , ( "failure", fail "",   Nothing )
  ]

reqIOTest (label, x, expect) = TestLabel label $ TestCase $ do
  val <- runRequest $ reqIO x
  assertEqual "" expect val
