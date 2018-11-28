module Main where

import Control.Monad
import System.Exit
import Test.HUnit

import qualified Spec.AndReq as AndReq

main = do
  counts <- runTestTT tests
  when (failures counts > 0 || errors counts > 0)
    exitFailure

tests = TestList [AndReq.tests]
