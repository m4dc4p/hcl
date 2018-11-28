module Spec.Common where

import Test.HUnit

import System.Console.HCL

req x = case x of
  Just x  -> makeReq x
  Nothing -> reqFail
