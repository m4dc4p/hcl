module Main where

import Control.Monad
import System.Exit
import Test.HUnit

import qualified Spec.AndReq as AndReq
import qualified Spec.Constructors as Constructors
import qualified Spec.NotReq as NotReq
import qualified Spec.OrReq as OrReq
import qualified Spec.ReqIf as ReqIf
import qualified Spec.ReqLift as ReqLift

main = do
  counts <- runTestTT tests
  when (failures counts > 0 || errors counts > 0)
    exitFailure

tests = TestList
  [ Constructors.tests
  , AndReq.tests
  , OrReq.tests
  , NotReq.tests
  , ReqIf.tests
  , ReqLift.tests
  ]
