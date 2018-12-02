module Spec.MonadPlus (tests) where

import Control.Monad
import Test.HUnit

import System.Console.HCL

tests = TestLabel "MonadPlus" $ TestList
  [ mzeroTest
  , mplusTests
  ]

mzeroTest = TestLabel "mzero" $ TestCase $ do
  val <- runRequest mzero
  assertEqual "" Nothing (val :: Maybe ())

mplusTests = TestLabel "mplus" $ TestList $ map mplusTest
  [ ( "both pass",    makeReq 1,         makeReq 2, Just 1  )
  , ( "first fails",  reqFail,           makeReq 1, Just 1  )
  , ( "first errors", Request $ fail "", makeReq 1, Just 1  )
  , ( "second fails", makeReq 1,         reqFail,   Just 1  )
  , ( "both fail",    reqFail,           reqFail,   Nothing )
  ]

mplusTest (label, x, y, expect) = TestLabel label $ TestCase $ do
  val <- runRequest $ x `mplus` y
  assertEqual "" expect val
