module Main where

import Control.Monad
import System.Exit
import Test.HUnit

main = do
  counts <- runTestTT tests
  when (failures counts > 0 || errors counts > 0)
    exitFailure

tests = TestList []
