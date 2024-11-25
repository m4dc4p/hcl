{-# LANGUAGE ScopedTypeVariables #-}

module Spec.MonadCatch (tests) where

import Control.Monad.Catch
import Test.HUnit

import System.Console.HCL

tests = TestLabel "Control.Monad.Catch" $ TestList
  [ catchTest
  , noThrowTest
  ]

catchTest = TestLabel "catch" $ TestCase $ do
  val <- runRequest $ catch (throwM TestException >> return False) $
    \(_ :: TestException) -> return True
  assertEqual "" (Just True) val

noThrowTest = TestLabel "catch" $ TestCase $ do
  val <- runRequest $ catch (return False) $
    \(_ :: TestException) -> return True
  assertEqual "" (Just False) val

data TestException = TestException deriving Show
instance Exception TestException
