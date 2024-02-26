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
     Ann body bodyT -> do 
        checkType env body bodyT;
        if bodyT == t 
        then Right expr
        else Left (incorrectTypeError expr t)
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
      App fn arg -> do 
        (_, fnT) <- synthType env fn 
        case fnT of 
          FunT argT outT -> do 
            checkType env arg argT;
            Right (expr, outT)
          o -> Left (incorrectFnTypeSynthesisError expr o)
      StrLit _ -> Right (expr, StrT)
      UnitLit -> Right (expr, UnitT)
      Ann annExpr annT -> do 
        checkType env annExpr annT;
        Right (expr, annT)
      IOBind fnExpr argExpr -> do
        (_, fnType) <- synthType env fnExpr;  
        case fnType of
          -- Check that the fnExpr is a function type
          FunT argT outT -> do 
            case outT of 
              -- Check that the outT is an IOT type
              IOT outTUnwrapped -> do
                -- Check that argument is argT wrapped in IOT
                checkType env argExpr (IOT argT);
                Right (expr, outT)
              o -> Left (incorrectTypeError expr o)
          o -> Left (incorrectFnTypeSynthesisError expr o)
      IOReturn retExpr -> do 
        (_, exprT)<- synthType env retExpr;
        Right (expr, IOT exprT)

