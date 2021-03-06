module Main where

import System.Environment
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

import Lang3Test (lang3Test)
import Lang4Test (lang4Test)
import ReaderTest (readerTest)
import StateTest (stateTest)
import Parser1Test (parser1Test)
import Parser2Test (parser2Test)
import Parser3Test (parser3Test)
import Parser4Test (parser4Test)

main = 
  do setEnv "TASTY_TIMEOUT" "60s"
     setEnv "TASTY_QUICKCHECK_TESTS" "100" 
     setEnv "TASTY_QUICKCHECK_MAX_SIZE" "100"
     defaultMain allTests
     unsetEnv "TASTY_TIMEOUT"
     unsetEnv "TASTY_QUICKCHECK_TESTS"
     unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"

allTests = testGroup "all tests" [
  lang3Test,
  lang4Test,
  readerTest,
  stateTest,
  parser1Test,
  parser2Test,
  parser3Test,
  parser4Test
  ]
