module TypeCheckerTests where 

import Test.HUnit
import Data.Map (Map, lookup, insert)
import Data.Either
import qualified Data.Map as Map
import DataTypes (Expr (..), Type (..))
import TypeCheck (checkType)


-- StrLit

strLitExpr :: Test
strLitExpr = TestCase 
  (let expr = StrLit "hello" in 
    assertEqual "StrLit should be StrT"
      (checkType Map.empty expr StrT)
      (Right expr))

-- Var errors
varExprEnvErr :: Test
varExprEnvErr = TestCase 
  (assertBool "Var is not referenced in environment"
    (isLeft 
      (checkType Map.empty (Var "x") StrT)))

varExprMapErr :: Test
varExprMapErr = TestCase 
  (assertBool "Var is wroncg type in environment" 
    (isLeft 
      (checkType (Map.fromList [("x", UnitT)]) (Var "x") StrT)))

varExprIOWrapErr :: Test
varExprIOWrapErr = TestCase 
  (assertBool "Environment wraps type in IOT" 
    (isLeft 
      (checkType (Map.fromList [("x", IOT StrT)]) (Var "x") StrT)))

-- Var valid case
varExprStrT :: Test
varExprStrT = TestCase (let expr = Var "x" in 
    assertEqual "Var should be found as StrT in the environment"  
      (checkType (Map.fromList [("x", StrT)]) expr StrT) 
      (Right expr))

varExprUnitT :: Test
varExprUnitT = TestCase (let expr = Var "x" in 
    assertEqual "Var should be found as UnitT in the environment"  
      (checkType (Map.fromList [("x", UnitT)]) expr UnitT) 
      (Right expr))

-- UnitLit
unitLitExpr :: Test
unitLitExpr = TestCase 
  (let expr = UnitLit in 
    assertEqual "UnitLit should be UnitT"  
      (checkType Map.empty expr UnitT) 
      (Right expr))

-- IOReturn 
testIOReturnExpr :: Test
testIOReturnExpr = TestCase 
  (let expr = IOReturn UnitLit in 
    assertEqual "IOReturn of UnitT should be IOT UnitT"  
      (checkType Map.empty expr (IOT UnitT)) 
      (Right expr))

testIOReturnExprNoUnwrap :: Test
testIOReturnExprNoUnwrap = TestCase 
  (let expr = IOReturn UnitLit in 
    assertBool "UnitT is not a valid IOReturn UnitLit"  
      (isLeft 
        (checkType Map.empty expr UnitT)))

-- Lam
testUnAnnotatedLam :: Test
testUnAnnotatedLam = TestCase 
  (let expr = Lam "x" (Var "x") in 
    assertEqual "Unannotated Lam should pass"
        (checkType Map.empty expr (FunT StrT StrT))
        (Right expr))

testAnnotatedLam :: Test 
testAnnotatedLam = TestCase 
  (let expr = Ann (Lam "x" (Var "x")) (FunT StrT StrT) in 
    assertEqual "Annotated Lam should pass"
      (checkType Map.empty expr (FunT StrT StrT))
      (Right expr))

testAnnotatedLamIgnoreArg :: Test 
testAnnotatedLamIgnoreArg = TestCase 
  (let expr = Ann (Lam "x" (StrLit "x")) (FunT UnitT StrT) in 
    assertEqual "Unused Argument but correct type should pass"
      (checkType Map.empty expr (FunT UnitT StrT))
      (Right expr))

testUnAnnotatedLamIgnoreArg :: Test 
testUnAnnotatedLamIgnoreArg = TestCase 
  (let expr = Lam "x" (StrLit "x") in 
    assertEqual "Unused Argument but correct type should pass"
      (checkType Map.empty expr (FunT UnitT StrT))
      (Right expr))

testNestedLam :: Test 
testNestedLam = TestCase 
  (let expr = Lam "x" (Lam "y" (Var "x")) in 
    assertEqual "Nested Annotated Lam should pass"
      (checkType Map.empty expr (FunT StrT (FunT StrT StrT)))
      (Right expr))

testWrongCheckTypeInLam :: Test 
testWrongCheckTypeInLam = TestCase 
  (let expr = Lam "x" (Lam "y" (Var "x")) in 
    assertBool "Nested Annotated Lam should pass"
      (isLeft 
        (checkType Map.empty expr (IOT (FunT StrT StrT)))))

-- Ann 
testIOReturnLamExpr:: Test
testIOReturnLamExpr = TestCase 
  (let expr = IOReturn (Ann (Lam "x" (Var "x")) (FunT StrT StrT)) in 
    assertEqual "UnitT is not a valid IOReturn UnitLit"  
      (checkType Map.empty expr (IOT (FunT StrT StrT)))
      (Right expr))

testAnnotatedIOReturnLamExpr:: Test
testAnnotatedIOReturnLamExpr = TestCase 
  (let expr = Ann (IOReturn (Lam "x" (Var "x"))) (IOT (FunT StrT StrT)) in 
    assertEqual "UnitT is not a valid IOReturn UnitLit"  
      (checkType Map.empty expr (IOT (FunT StrT StrT)))
      (Right expr))

testFalseAnnotatedIOReturnLamExpr:: Test
testFalseAnnotatedIOReturnLamExpr = TestCase 
  (let expr = Ann (IOReturn (Lam "x" (Var "x"))) (FunT StrT StrT) in 
    assertBool "UnitT is not a valid IOReturn UnitLit"  
      (isLeft 
        (checkType Map.empty expr (IOT (FunT StrT StrT)))))

testNestedAnnotation :: Test 
testNestedAnnotation = TestCase 
  (let 
      t = FunT StrT (FunT StrT (FunT StrT StrT))
      expr = Ann (Lam "x" (Lam "y" (Lam "z" (Var "x")))) t
    in 
    assertEqual "Annotate a nested lambda"
      (checkType Map.empty expr t)
      (Right expr))


testNestedAnnotationWithFunArg :: Test 
testNestedAnnotationWithFunArg = TestCase 
  (let 
      t = FunT (FunT StrT StrT) (FunT (FunT StrT StrT) (FunT StrT StrT))
      expr = Ann (Lam "x" (Lam "y" (Lam "z" (App (Var "x") (Var "z"))))) t
    in 
    assertEqual "Annotate a nested lambda with 2 function arguments and 1 string argument"
      (checkType Map.empty expr t)
      (Right expr))

testNestedAnnotationWithFunRet :: Test 
testNestedAnnotationWithFunRet = TestCase 
  (let 
      t = FunT (FunT StrT StrT) (FunT (FunT StrT StrT) (FunT StrT (FunT StrT StrT)))
      expr = Ann (Lam "x" (Lam "y" (Lam "z" (Var "x")))) t
    in 
    assertEqual "Annotate a nested lambda with 2 function arguments and 1 string argument"
      (checkType Map.empty expr t)
      (Right expr))

-- App

testEnvLamApp :: Test
testEnvLamApp = TestCase 
  (let  expr = App (Var "lam") (StrLit "x")
        env = Map.fromList [("lam", FunT StrT StrT)]
    in 
    assertEqual "Application of lambda from environment is valid"  
        (checkType env expr StrT)
        (Right expr))

testAnnLamApp :: Test 
testAnnLamApp = TestCase 
  (let  expr = App (Ann (Lam "arg" (Var "arg")) (FunT StrT StrT)) (StrLit "x")
        env = Map.empty
    in 
    assertEqual "Application of annotated lambda is valid"  
        (checkType env expr StrT)
        (Right expr))

testUnAnnotatedLamApp :: Test 
testUnAnnotatedLamApp = TestCase
  (let  expr = App (Lam "arg" (Var "arg")) (StrLit "x")
        env = Map.empty
    in 
    assertBool "Unannotated Lam cannot be applied"  
      (isLeft 
        (checkType env expr StrT)))

testIncorrectEnvType :: Test 
testIncorrectEnvType = TestCase
  (let  expr = App (Var "somefun") (StrLit "x")
        env = Map.fromList [("someFun", StrT)]
    in 
    assertBool "Cannot apply a StrT from environment"  
      (isLeft 
        (checkType env expr StrT)))


testIncorrectArgType :: Test 
testIncorrectArgType = TestCase
  (let  expr = App (Var "somefun") UnitLit
        env = Map.fromList [("someFun", FunT StrT UnitT)]
    in 
    assertBool "Cannot pass UnitLit as arg to lam taking str"  
      (isLeft 
        (checkType env expr StrT)))

testApplyingAnIOBind :: Test 
testApplingAnIOBind = TestCase 
  (let 
    expr = App (IOBind (Ann (Lam "x" (IOReturn (Var "x"))) (FunT StrT (IOT StrT)))) (IOReturn StrT)
  in 
    assertBool "Cannot partially apply an IOBind"
      (isLeft (checkType env expr (IOT StrT))

-- IOBind
testIOBind :: Test 
testIOBind = TestCase
  (let  expr = IOBind (Ann (Lam "x" (IOReturn (Var "x"))) (FunT StrT (IOT StrT))) (IOReturn (StrLit "hello"))
        env = Map.empty
    in 
    assertEqual "Cannot pass UnitLit as arg to lam taking str"  
        (checkType env expr (IOT StrT))
        (Right expr))

testIOBindIncorrectReturn :: Test 
testIOBindIncorrectReturn = TestCase
  (let  
      expr = IOBind (Ann (Lam "x" (Var "x")) (FunT StrT StrT)) (IOReturn (StrLit "hello"))
      env = Map.empty
    in 
    assertBool "Function passed to IOBind should be an IOReturn"  
      (isLeft 
        (checkType env expr StrT)))

testIOBindIncorrectArg :: Test 
testIOBindIncorrectArg = TestCase
  (let  expr = IOBind (Ann (Lam "x" (IOReturn (Var "x"))) (FunT StrT (IOT StrT))) (StrLit "hello")
        env = Map.empty
    in 
    assertBool "Argument passed to an IOBind must be an IOT"  
        (isLeft (checkType env expr StrT)))


testIOBindIncorrectFunctionAnnotation :: Test 
testIOBindIncorrectFunctionAnnotation = TestCase
  (let  expr = IOBind (Ann (Lam "x" (IOReturn (StrLit "x"))) (FunT StrT (IOT UnitT))) (IOReturn (StrLit "hello"))
        env = Map.empty
    in 
    assertBool "Must typecheck the return from an IOBind"  
        (isLeft (checkType env expr StrT)))

testIOBindWithEnvVars :: Test 
testIOBindWithEnvVars  = TestCase 
  (let  expr = IOBind (Var "ioBindFun") (Var "ioval")
        env = Map.fromList [("ioBindFun", FunT StrT (IOT StrT)), ("ioval", IOT StrT)]
    in 
      assertEqual "IO Bind should do typechecking using only env variabiales"
        (checkType env expr (IOT StrT))
        (Right expr))

testIOBindWithIncorrectEnvVars :: Test 
testIOBindWithIncorrectEnvVars  = TestCase 
  (let  expr = IOBind (Var "ioBindFun") (Var "ioval")
        env = Map.fromList [("ioBindFun", FunT UnitT (IOT StrT)), ("ioval", IOT StrT)]
    in 
      assertBool "IO Bind should do typechecking using only env variabiales"
        (isLeft (checkType env expr (IOT StrT))))

-- Synth test


huTests :: Test 
huTests = TestList [
    strLitExpr
  , varExprEnvErr
  , varExprMapErr
  , varExprIOWrapErr
  , varExprStrT
  , varExprUnitT
  , unitLitExpr
  , testIOReturnExprNoUnwrap
  , testIOReturnExpr
  , testIOReturnLamExpr
  , testAnnotatedLam
  , testUnAnnotatedLam
  , testAnnotatedLamIgnoreArg
  , testUnAnnotatedLamIgnoreArg
  , testNestedAnnotation
  , testNestedAnnotationWithFunArg
  , testNestedAnnotationWithFunRet
  , testNestedLam
  , testAnnotatedIOReturnLamExpr
  , testFalseAnnotatedIOReturnLamExpr
  , testEnvLamApp
  , testAnnLamApp
  , testUnAnnotatedLamApp
  , testIncorrectArgType
  , testIncorrectEnvType
  , testIOBindIncorrectArg
  , testIOBindIncorrectReturn
  , testIOBindIncorrectFunctionAnnotation
  , testIOBindWithEnvVars
  , testWrongCheckTypeInLam
  ]
