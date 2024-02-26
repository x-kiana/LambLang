{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
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

data TypeError 
  = TypeSynthesisError {expr :: !Expr}
  | TypeCheckError { expr :: !Expr, expected_type :: !Type }
  | UnboundTypeError { str :: !String}
  | LambdaSynthesisError {expr :: !Expr}
  | ExpectedWrappedValueError {expr :: !Expr, actualType:: !Type}
  | ExpectedFunctionError {expr :: !Expr, actualType :: !Type}
  deriving (Eq)

instance Show TypeError where 
  show err = 
    case err of 
      TypeSynthesisError {expr} -> show "Failed to synthesize expression " ++ show expr ++ " to a type"
      TypeCheckError { expr, expected_type } -> "Failed to evaluate expression " ++ show expr ++ " to the type " ++ show expected_type
      UnboundTypeError { str } -> "Found variable with undeclared type: " ++ str 
      LambdaSynthesisError { expr } -> "Cannot synthesize the lambda " ++ show expr
      ExpectedWrappedValueError { expr, actualType } -> "Expected value's type in expression " ++ show expr ++ " to be IOT, but got type " ++ show actualType
      ExpectedFunctionError { expr , actualType } -> "Expected expression " ++ show expr ++ " to be a function, but got " ++ show actualType


{- Helper function to typeCheck
 -}
checkType :: TypeEnv -> Expr -> Type -> Either TypeError Expr
checkType env expr t = 
  case expr of 
    Lam str body-> 
      case t of 
        FunT argT bodyT -> do 
          checkType (insert str argT env) body bodyT;
          Right expr
        o -> Left (TypeCheckError expr t)
    Ann body bodyT -> do 
      checkType env body bodyT;
      matchExpectedType t bodyT expr
    IOReturn body ->
      (case t of 
          IOT rt -> do 
            checkType env body rt;
            Right expr
          o -> Left (TypeCheckError expr t))
    App fnExpr argExpr ->
      case synthType env expr of 
          Right (_, exprT) -> 
            matchExpectedType t exprT expr
          Left _ -> do
            (_, argT) <- synthType env argExpr
            checkType env fnExpr (FunT argT t);
            Right expr
    IOBind argExpr fnExpr -> 
       case synthType env expr of
         Right (_, exprT) -> 
            matchExpectedType t exprT expr
         Left _  -> do 
            (_, wrappedArgT) <- synthType env argExpr;
            case (wrappedArgT, t) of 
              (IOT argT, IOT outT) -> do 
                checkType env fnExpr (FunT argT t);
                Right expr
              (IOT _, errT) -> Left (ExpectedWrappedValueError expr errT)
              (errT, _ ) -> Left (ExpectedWrappedValueError expr errT)
    _ -> do 
     (_, exprT) <- synthType env expr
     matchExpectedType t exprT expr
  where 
    matchExpectedType expected actual expr = 
     if expected == actual
     then Right expr 
     else 
       Left (TypeCheckError expr expected)



{- Given a TypeEnv and an expression, returns Right (Expr, Type)  if the expression is 
 - well typed. The returned type will be the type of the expression. Otherwise, an error 
 - is returned.
 -}
synthType :: TypeEnv -> Expr -> Either TypeError (Expr, Type)
synthType env expr = 
  case expr of
      Var str -> case Map.lookup str env of 
        (Just t) -> Right (expr, t)
        Nothing -> Left (UnboundTypeError str)
      Lam arg body ->  Left (LambdaSynthesisError expr)
      App fn arg -> do 
        (_, fnT) <- synthType env fn 
        case fnT of 
          FunT argT outT -> do 
            checkType env arg argT;
            Right (expr, outT)
          o -> Left (ExpectedFunctionError expr fnT)
      StrLit _ -> Right (expr, StrT)
      UnitLit -> Right (expr, UnitT)
      Ann annExpr annT -> do 
        checkType env annExpr annT;
        Right (expr, annT)
      IOBind argExpr fnExpr -> do
        (_, t) <- synthType env fnExpr;  
        case t of
          -- Check that the fnExpr is a function type
          FunT argT outT -> do 
            case outT of 
              -- Check that the outT is an IOT type
              IOT outTUnwrapped -> do
                -- Check that argument is argT wrapped in IOT
                checkType env argExpr (IOT argT);
                Right (expr, outT)
              _ -> Left (ExpectedWrappedValueError expr outT)
          _ -> Left (ExpectedFunctionError expr t)
      IOReturn retExpr -> do 
        (_, exprT)<- synthType env retExpr;
        Right (expr, IOT exprT)


