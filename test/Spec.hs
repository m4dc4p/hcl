module Main where

import Control.Monad
import System.Exit
import Test.HUnit

import qualified Spec.AndReq as AndReq
import qualified Spec.Constructors as Constructors
import qualified Spec.Monad as Monad
import qualified Spec.MonadCatch as MonadCatch
import qualified Spec.MonadPlus as MonadPlus
import qualified Spec.NotReq as NotReq
import qualified Spec.OrReq as OrReq
import qualified Spec.ReqAgree as ReqAgree
import qualified Spec.ReqDefault as ReqDefault
import qualified Spec.ReqIf as ReqIf
import qualified Spec.ReqLift as ReqLift
import qualified Spec.ReqLift2 as ReqLift2
import qualified Spec.ReqMaybe as ReqMaybe
import qualified Spec.ReqWhich as ReqWhich

main = do
  counts <- runTestTT tests
  when (failures counts > 0 || errors counts > 0)
    exitFailure

tests = TestList
  [ Constructors.tests
  , AndReq.tests
  , OrReq.tests
  , NotReq.tests
  , ReqDefault.tests
  , ReqMaybe.tests
  , ReqWhich.tests
  , ReqIf.tests
  , ReqLift.tests
  , ReqLift2.tests
  , ReqAgree.tests
  , Monad.tests
  , MonadCatch.tests
  , MonadPlus.tests
  ]
