module Spec.ReqAgree (tests) where

import Test.HUnit

import System.Console.HCL

tests = TestLabel "reqAgree" $ TestList $ map test'
  [ ( "yes",  Nothing,    Just True  )
  , ( " yes", Nothing,    Just True  )
  , ( "Yes",  Nothing,    Just True  )
  , ( "yes",  Just False, Just True  )
  , ( "no",   Nothing,    Just False )
  , ( " no",  Nothing,    Just False )
  , ( "No",   Nothing,    Just False )
  , ( "no",   Just True,  Just False )
  , ( "foo",  Nothing,    Nothing    )
  , ( "foo",  Just True,  Just True  )
  , ( "foo",  Just False, Just False )
  ]

test' (str, def, expect) = TestLabel label $ TestCase assertion where
  label = "input = " ++ show str ++ ", default = " ++ show def
  assertion = do
    val <- runRequest $ reqAgree def $ return str
    assertEqual "" val expect
