module Main where

import System.IO
import DataTypes
import Parse
import Eval
import TypeCheck

run :: String -> Either String Expr
run s = do
  exp <- parse s
  either (Left . show) Right (checkType defaultTypeEnv exp (IOT UnitT))


main :: IO ()
main = do
  putStr "LambLang> "
  hFlush stdout
  line <- getLine
  case run line of
    Left msg -> print msg
    Right e -> eval e
  hFlush stdout
  putStrLn ""
  main
