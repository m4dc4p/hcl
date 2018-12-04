module Spec.Monad (tests) where

import Test.HUnit

import System.Console.HCL

tests = TestLabel "Monad" $ TestList [ returnTest, bindTests ]

returnTest = TestLabel "return" $ TestCase $ do
  val <- runRequest $ return 1
  assertEqual "" (Just 1) val

bindTests = TestLabel ">>=" $ TestList $ map bindTest
  [ ( "success", Just 1,  Just 2  )
  , ( "failure", Nothing, Nothing )
  ]

bindTest (label, x, expect ) = TestLabel label $ TestCase $ do
  val <- runRequest $ reqLiftMaybe x >>= return . succ
  assertEqual "" expect val
