-- | Test Module

module Main where

import           Macros
import           Test.HUnit

test1 = TestCase
  (   (Right "3 + 2 + 4")
  @=? (createDef ["arg1", "arg2", "arg3"] "arg1 + arg2 + arg3" ["3", "2", "4"]
      )
  )

test2 = TestCase
  (   (Right "4 + 4 + 4")
  @=? (createDef ["arg1", "arg2", "arg3"] "arg3 + arg3 + arg3" ["3", "2", "4"]
      )
  )

test3 = TestCase
  (   (Left "Definition has less arguments than expected")
  @=? (createDef [] "20" ["3", "2"])
  )

tests = TestList
  [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]

main = do
  counts <- runTestTT tests
  putStrLn $ showCounts counts
