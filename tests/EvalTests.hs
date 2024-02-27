module EvalTests where

import Eval
import Test.HUnit
import DataTypes

varExpr :: Test
varExpr = TestCase
  (let expr = (Var "x") in
    assertEqual "x does not exist in the initial environment"
    (eval' initEnv expr)
    (Left "Unbound variable"))

varPrintStr :: Test
varPrintStr = TestCase
  (let expr = (Var "printStr") in
    assertEqual "printStr is in the initial environment"
    (show (eval' initEnv expr))
    ("Right <procedure>"))

varReadStr:: Test
varReadStr = TestCase
  (let expr = (Var "readStr") in
    assertEqual "readStr is in the initial environment"
    (show (eval' initEnv expr))
    ("Right <IO>"))

lamExpr :: Test
lamExpr = TestCase
  (let expr = (Lam "x" (Var "x")) in
    assertEqual "Lam represents a functions"
    (show (eval' initEnv expr))
    ("Right <procedure>"))

appExpr :: Test
appExpr = TestCase
  (let expr = (App (Lam "x" (Var "x")) (StrLit "hi")) in
    assertEqual "App consists of a Lam and the arg it's applied to"
    (eval' initEnv expr)
    (Right (StrV "hi")))

appExprFalseFn :: Test
appExprFalseFn = TestCase
  (let expr = (App (Var "readStr") (Var "readStr")) in
    assertEqual "App expects a function as its first argument"
    (eval' initEnv expr)
    (Left "Expected a function value"))

appExprUnbound :: Test
appExprUnbound = TestCase
  (let expr = (App (Var "x") (Var "x")) in
    assertEqual "App expects a function as its first argument"
    (eval' initEnv expr)
    (Left "Unbound variable"))

appExprFalseArg :: Test
appExprFalseArg = TestCase
  (let expr = (App (Var "printStr") (Var "x")) in
    assertEqual "App expects its argument to evaluate to a value"
    (eval' initEnv expr)
    (Left "Unbound variable"))

strlitExpr :: Test
strlitExpr = TestCase
  (let expr = (StrLit "hello, world!") in
    assertEqual "StrLit is a StrV"
    (eval' initEnv expr)
    (Right (StrV "hello, world!")))

unitlitExpr :: Test
unitlitExpr = TestCase
  (let expr = UnitLit in
    assertEqual "UnitLit is a UnitV"
    (eval' initEnv expr)
    (Right UnitV))

annExpr :: Test
annExpr = TestCase
  (let expr = (Ann (Lam "x" (Var "x")) (FunT StrT StrT)) in
    assertEqual "Annotations don't effect evaluation"
    (show (eval' (envPut "x" (StrV "hi") initEnv) expr))
    ("Right <procedure>"))

annExprBad :: Test
annExprBad = TestCase
  (let expr = (Ann (Var "x") (StrT)) in
    assertEqual "if the annotated expression throws an error, we throw the error"
    (eval' initEnv expr)
    (Left "Unbound variable"))

ioBindExpr :: Test
ioBindExpr = TestCase
  (let expr = (IOBind (IOReturn (StrLit "hello")) (Lam "x" (Var "x"))) in
    assertEqual "IOBind needs and IOV and FunV"
    (show (eval' initEnv expr))
    ("Right <IO>"))

ioBindExprBadFn :: Test
ioBindExprBadFn = TestCase
  (let expr = (IOBind (IOReturn (StrLit "hello")) (Var "readStr")) in
    assertEqual "IOBind needs a function as its second argument"
    (eval' initEnv expr)
    (Left "Expected the second argument to IOBind to be a FunV"))

ioBindExprBadIO :: Test
ioBindExprBadIO = TestCase
  (let expr = (IOBind (StrLit "hello") (Lam "x" (Var "x"))) in
    assertEqual "IOBind needs an IO as its first argument"
    (eval' initEnv expr)
    (Left "Expected the first argument to IOBind to be a IO Value"))

ioReturnExpr :: Test
ioReturnExpr = TestCase
  (let expr = (IOReturn (StrLit "hello")) in
    assertEqual "IOReturn returns the val wrapped in IO"
    (show (eval' initEnv expr))
    ("Right <IO>"))

huTests :: Test
huTests = TestList [
  varExpr
  , varPrintStr
  , varReadStr
  , lamExpr
  , appExpr
  , appExprFalseFn
  , appExprUnbound
  , appExprFalseArg
  , strlitExpr
  , unitlitExpr
  , annExpr
  , annExprBad
  , ioBindExpr
  , ioBindExprBadFn
  , ioBindExprBadIO
  , ioReturnExpr]
