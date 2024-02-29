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
  isEof <- hIsEOF stdin
  if isEof
  then return ()
  else do
    line <- hGetLine stdin
    case run line of
      Left msg -> hPutStr stdout msg
      Right e -> eval e
    putStrLn ""
    hFlush stdout
    main
