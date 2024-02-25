{-# LANGUAGE OverloadedStrings #-}
module TypeCheck where 
import Data.Map (Map, lookup, insert)
import qualified Data.Map as Map
import DataTypes (Expr (..), Type (..))

type TypeEnv = Map String Type


{- Given an expression and a type, returns Right Expr if the Expression matches the type. Returns
 - an error otherwise
 -}
-- typeCheck :: Expr -> Type -> Either String Expr
-- typeCheck = undefined


checkExpectedType :: Expr -> Type -> Type -> Either String Expr
checkExpectedType expr expected actual = 
  if expected == actual 
  then Right expr 
  else 
    Left (incorrectTypeError expr expected)

incorrectTypeError :: Expr -> Type -> String
incorrectTypeError expr expected ="Incorrect type for expression " ++ show expr ++ " expected " ++ show expected


incorrectFnTypeSynthesisError :: Expr -> Type -> String 
incorrectFnTypeSynthesisError expr t = "Expected function type for expression " ++ show expr ++ " but got type: " ++ show t


{- Helper function to typeCheck
 -}
checkType :: TypeEnv -> Expr -> Type -> Either String Expr
checkType env expr t = 
  case expr of 
     Lam str body-> 
        case t of 
          FunT argT bodyT -> do 
            checkType (insert str argT env) body bodyT;
            Right expr
          o -> Left (incorrectTypeError expr t)
     App fn arg -> do 
        (_, fnT) <- synthType env fn 
        case fnT of 
          FunT argT bodyT -> do 
            checkType env arg argT;
            Right expr
          o -> Left (incorrectFnTypeSynthesisError expr o)
     Ann body bodyT -> do 
        checkExpectedType expr t bodyT;
        checkType env body t;
        Right expr
      --   Data: IOBind (A -> IOT B) (IOT A)  :: IOT B       IOBind fn arg -> 
     IOBind fn arg -> do
        (_, fnT) <- synthType env fn 
        case fnT of 
          FunT argT bodyT -> 
            case bodyT of 
              IOT bodyTUnwrapped -> 
                do
                -- since this is an IOBind, the argument type is wrapped in an IOT
                checkType env arg (IOT argT);
                Right expr
              o -> Left (incorrectTypeError expr o)
          o -> Left (incorrectFnTypeSynthesisError expr o)
     IOReturn body ->  
        case t of 
          IOT rt -> do 
            checkType env body rt;
            Right expr
          o -> Left (incorrectTypeError expr t)
     _ -> do 
      (_, exprT) <- synthType env expr
      if t == exprT
      then Right expr 
      else 
        Left (incorrectTypeError expr t)



{- Given a TypeEnv and an expression, returns Right (Expr, Type)  if the expression is 
 - well typed. The returned type will be the type of the expression. Otherwise, an error 
 - is returned.
 -}
synthType :: TypeEnv -> Expr -> Either String (Expr, Type)
synthType env expr = 
  case expr of
      Var str -> case Map.lookup str env of 
        (Just t) -> Right (expr, t)
        Nothing -> Left ("Unbound variable " ++ show str ++ " has no type.")
      Lam arg body ->  Left ("Cannot synthesize lambda " ++ show expr)
      App fnExpr argExpr -> do 
        (_, fnType) <- synthType env fnExpr;
        let (FunT argT outT) = fnType in do 
          checkType  env argExpr argT;
          Right (expr, outT)
      StrLit _ -> Right (expr, StrT)
      UnitLit -> Right (expr, UnitT)
      Ann annExpr annT -> do 
        checkType env annExpr annT;
        Right (expr, annT)
      IOBind fnExpr argExpr -> do
        (_, fnType) <- synthType env fnExpr;  
        let (FunT argT outT) = fnType in do 
          checkType env argExpr argT;
          Right (expr, outT)
      IOReturn retExpr -> do 
        (_, exprT)<- synthType env retExpr;
        Right (expr, IOT exprT)


