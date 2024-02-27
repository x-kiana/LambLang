module Main where

import Test.HUnit
import qualified System.Exit as Exit
import TypeCheckerTests (huTests)


test1 :: Test
test1 = TestCase (assertEqual "should return 3" 3 (3 + 0))

tests :: Test 
tests = TestList [TestLabel "Type Checker" test1, TestLabel "TypeChcekerTests" TypeCheckerTests.huTests]

main :: IO ()
main = do 
  result <- runTestTT tests 
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
