module Main where

import Test.HUnit
import qualified System.Exit as Exit
import TypeCheckerTests (huTests)
import EvalTests (huTests)

tests :: Test 
tests = TestList [TestLabel "EvalTests" EvalTests.huTests, TestLabel "TypeChcekerTests" TypeCheckerTests.huTests]

main :: IO ()
main = do 
  result <- runTestTT tests 
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
