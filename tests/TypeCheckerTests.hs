module TypeCheckerTests where 

import Test.HUnit
import Data.Map (Map, lookup, insert)
import Data.Either
import qualified Data.Map as Map
import DataTypes (Expr (..), Type (..))
import TypeCheck (checkType, synthType)


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
    assertEqual "Check that annotation works with IOT"  
      (checkType Map.empty expr (IOT (FunT StrT StrT)))
      (Right expr))

testFalseAnnotatedIOReturnLamExpr:: Test
testFalseAnnotatedIOReturnLamExpr = TestCase 
  (let expr = Ann (IOReturn (Lam "x" (Var "x"))) (FunT StrT StrT) in 
    assertBool "Annotation is false"  
      (isLeft 
        (checkType Map.empty expr (FunT StrT StrT))))

testFalseTypeCheckAnnotated :: Test
testFalseTypeCheckAnnotated = TestCase 
  (let expr = Ann (IOReturn (Lam "x" (Var "x"))) (IOT (FunT StrT StrT)) in 
    assertBool "UnitT is not a valid IOReturn UnitLit"  
      (isLeft 
        (checkType Map.empty expr (FunT StrT StrT))))

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

testApplyingWithAnnotation :: Test 
testApplyingWithAnnotation = TestCase 
  (let 
    expr = App (Ann (Lam "x" (IOReturn (Var "x"))) (FunT StrT (IOT StrT))) (StrLit "hi")
  in 
    assertEqual "Apply should work with annotated lambda"
      (checkType Map.empty expr (IOT StrT))
      (Right expr))


testApplyingWithNoFunctionPassed :: Test 
testApplyingWithNoFunctionPassed = TestCase 
  (let 
    expr = App (StrLit "hello") (StrLit "hi")
  in 
    assertBool "Apply cannot take StrLit in the function position"
      (isLeft (checkType Map.empty expr (IOT StrT))))

-- IOBind
testIOBind :: Test 
testIOBind = TestCase
  (let  expr = IOBind (IOReturn (StrLit "hello")) (Ann (Lam "x" (IOReturn (Var "x"))) (FunT StrT (IOT StrT)))
        env = Map.empty
    in 
    assertEqual "Cannot pass UnitLit as arg to lam taking str"  
        (checkType env expr (IOT StrT))
        (Right expr))

testIOBindIncorrectReturn :: Test 
testIOBindIncorrectReturn = TestCase
  (let  
      expr = IOBind (IOReturn (StrLit "hello")) (Ann (Lam "x" (Var "x")) (FunT StrT StrT))
      env = Map.empty
    in 
    assertBool "Function passed to IOBind should be an IOReturn"  
      (isLeft 
        (checkType env expr StrT)))

testIOBindIncorrectArg :: Test 
testIOBindIncorrectArg = TestCase
  (let  expr = IOBind (StrLit "hello") (Ann (Lam "x" (IOReturn (Var "x"))) (FunT StrT (IOT StrT)))
        env = Map.empty
    in 
    assertBool "Argument passed to an IOBind must be an IOT"  
        (isLeft (checkType env expr StrT)))


testIOBindIncorrectFunctionAnnotation :: Test 
testIOBindIncorrectFunctionAnnotation = TestCase
  (let  expr = IOBind (IOReturn (StrLit "hello")) (Ann (Lam "x" (IOReturn (StrLit "x"))) (FunT StrT (IOT UnitT)))
        env = Map.empty
    in 
    assertBool "Must typecheck the return from an IOBind"  
        (isLeft (checkType env expr StrT)))

testIOBindWithEnvVars :: Test 
testIOBindWithEnvVars  = TestCase 
  (let  expr = IOBind (Var "ioval") (Var "ioBindFun")
        env = Map.fromList [("ioBindFun", FunT StrT (IOT StrT)), ("ioval", IOT StrT)]
    in 
      assertEqual "IO Bind should do typechecking using only env variabiales"
        (checkType env expr (IOT StrT))
        (Right expr))

testIOBindWithIncorrectEnvVars :: Test 
testIOBindWithIncorrectEnvVars  = TestCase 
  (let  expr = IOBind (Var "ioval") (Var "ioBindFun") 
        env = Map.fromList [("ioBindFun", FunT UnitT (IOT StrT)), ("ioval", IOT StrT)]
    in 
      assertBool "IO Bind should do typechecking using only env variabiales"
        (isLeft (checkType env expr (IOT StrT))))

testIOBindWithNonfunction :: Test 
testIOBindWithNonfunction = TestCase 
  (let  expr = IOBind (Var "ioval") (Var "ioBindFun")
        env = Map.fromList [("ioBindFun", StrT), ("ioval", IOT StrT)]
    in 
      assertBool "IO Bind must take FunT as first argument"
        (isLeft (checkType env expr (IOT StrT))))

{- Synth Tests -}
-- Synth Var
testSynthVarNoEnv :: Test
testSynthVarNoEnv = TestCase
  (let  expr = Var "str"
        env = Map.empty
    in 
    assertBool "Cannot Synthesize Var without Env"
      (isLeft
        (synthType env expr)))


testSynthVarIrrelevantEnv :: Test
testSynthVarIrrelevantEnv = TestCase
  (let  expr = Var "str"
        env = Map.fromList [("hi", StrT)]
    in 
    assertBool "Cannot Synthesize Var with irrelevant env"
      (isLeft
        (synthType env expr)))

testSynthVarCorrectEnv :: Test
testSynthVarCorrectEnv = TestCase
  (let  expr = Var "str"
        env = Map.fromList [("str", StrT)]
        t = StrT
    in 
    assertEqual "Must synthesize Var using env"
        (synthType env expr)
        (Right (expr, t)))
-- Synth Lam 
testSynthLam :: Test
testSynthLam = TestCase
  (let  expr = Lam "x" (StrLit "x")
        env = Map.empty
        t = StrT
    in 
    assertBool "Lam cannot be synthed"
        (isLeft 
          (synthType env expr)))

-- Synth App
testSynthApp :: Test
testSynthApp = TestCase
  (let  expr = App (Ann (Lam "x" (StrLit "x")) (FunT StrT StrT)) (StrLit "hello")
        env = Map.empty
        t = StrT
    in 
    assertEqual "Should be able to synth application of an annotated lambda"
        (synthType env expr)
        (Right (expr, t)))


testSynthAppFromEnv :: Test
testSynthAppFromEnv = TestCase
  (let  expr = App (Var "fun") (Var "val")
        env = Map.fromList [("fun", FunT StrT StrT), ("val", StrT)]
        t = StrT
    in 
    assertEqual "Should be able to synth application of env defined lambda"
        (synthType env expr)
        (Right (expr, t)))


testSynthAppErrFromEnv :: Test
testSynthAppErrFromEnv = TestCase
  (let  expr = App (Var "fun") (StrLit "hello")
        env = Map.fromList [("fun", FunT UnitT StrT)]
        t = StrT
    in 
    assertBool "Should be able to synth application of env defined lambda"
        (isLeft (synthType env expr)))

testSynthAppErrFromAnn :: Test
testSynthAppErrFromAnn = TestCase
  (let  expr = App (Ann UnitLit (FunT StrT StrT)) (StrLit "hello")
        env = Map.empty
        t = StrT
    in 
    assertBool "Should be able to synth application of env defined lambda"
        (isLeft (synthType env expr)))

-- Synth StrLit 

testSynthStrLit :: Test
testSynthStrLit = TestCase
  (let  expr = StrLit "text"
        env = Map.empty
        t = StrT
    in 
      assertEqual "Should be able to synth application of env defined lambda"
        (synthType env expr)
        (Right (expr, t)))

testSynthStrLitFromEnv :: Test
testSynthStrLitFromEnv = TestCase
  (let  expr = Var "text"
        env = Map.fromList [("text", StrT)]
        t = StrT
    in 
      assertEqual "Should be able to synth application of env defined lambda"
        (synthType env expr)
        (Right (expr, t)))

-- Synth UnitT
testSynthUnitLit :: Test
testSynthUnitLit = TestCase
  (let  expr = UnitLit
        env = Map.empty
        t = UnitT
    in 
      assertEqual "Should be able to synth application of env defined lambda"
        (synthType env expr)
        (Right (expr, t)))

testSynthUnitLitFromEnv :: Test
testSynthUnitLitFromEnv = TestCase
  (let  expr = Var "text"
        env = Map.fromList [("text", UnitT)]
        t = UnitT
    in 
      assertEqual "Should be able to synth application of env defined lambda"
        (synthType env expr)
        (Right (expr, t)))

-- Synth Ann
testSynthAnn :: Test
testSynthAnn = TestCase
  (let  
      t = FunT StrT StrT
      expr = Ann (Lam "hello" (Var "hello")) t
      env = Map.empty
    in 
      assertEqual "Should be able to synth application of env defined lambda"
        (synthType env expr)
        (Right (expr, t)))

-- Synth IOBind 
testSynthIOBind :: Test 
testSynthIOBind = TestCase
  (let 
      t = IOT StrT
      expr = IOBind (IOReturn (StrLit "text")) (Ann (Lam "x" (IOReturn (Var "x"))) (FunT StrT (IOT StrT)))
      env = Map.empty
    in 
      assertEqual "Should be able to synth application of env defined lambda"
        (synthType env expr)
        (Right (expr, t)))

testSynthBadIOBindFunErr :: Test 
testSynthBadIOBindFunErr = TestCase
  (let 
      t = FunT StrT (IOT StrT)
      expr = IOBind (IOReturn (StrLit "text")) (StrLit "x")
      env = Map.empty
    in 
      assertBool "Should be able to synth application of env defined lambda"
        (isLeft (synthType env expr)))

-- Synth IOReturn
testSynthIOReturn :: Test
testSynthIOReturn = TestCase 
  (let 
      t = IOT StrT
      expr = IOReturn (StrLit "Hello")
      env = Map.empty
    in 
      assertEqual "Should be able to synth application of env defined lambda"
        (synthType env expr)
        (Right (expr, t)))

testEnvSynthIOReturn = TestCase 
  (let 
      t = IOT StrT
      expr = IOReturn (Var "x")
      env = Map.fromList [("x", StrT)]
    in 
      assertEqual "Should be able to synth application of env defined lambda"
        (synthType env expr)
        (Right (expr, t)))

-- Synth Complex
testSynthComplex = TestCase 
  (let 
      t = IOT StrT
      expr = 
        App 
          (Ann 
            (Lam "x" 
              (IOBind
                (App (Var "scanf") UnitLit)
                (Lam "y" (IOReturn (App (Var "x") (Var "y"))))))
            (FunT (FunT StrT StrT) (IOT StrT)))
          (Lam "x" (Var "x"))
      env = Map.fromList [("scanf", FunT UnitT (IOT StrT))]
    in 
      assertEqual "Only top level annotation is required"
        (checkType env expr t)
        (Right expr))


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
  , testApplyingWithAnnotation
  , testApplyingWithNoFunctionPassed
  , testFalseTypeCheckAnnotated
  , testIOBindWithNonfunction
  {- Synth Tests -}
  , testSynthVarNoEnv
  , testSynthVarIrrelevantEnv
  , testSynthVarCorrectEnv
  , testSynthApp
  , testSynthAppFromEnv
  , testSynthAppErrFromEnv
  , testSynthAppErrFromAnn
  , testSynthStrLit
  , testSynthStrLitFromEnv
  , testSynthUnitLit 
  , testSynthUnitLitFromEnv
  , testSynthAnn
  , testSynthIOBind
  , testSynthBadIOBindFunErr
  , testSynthIOReturn
  , testEnvSynthIOReturn
  , testSynthComplex
  ]

