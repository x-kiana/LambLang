{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module TypeCheck where 
import Data.Map (Map, lookup, insert)
import qualified Data.Map as Map
import DataTypes (Expr (..), Type (..))

type TypeEnv = Map String Type


defaultTypeEnv = Map.fromList [("readStr", IOT StrT), ("printStr", FunT StrT (IOT UnitT))]

data TypeError 
  = TypeSynthesisError { expr :: !Expr }
  | TypeCheckError { expr :: !Expr, expected_type :: !Type, actual_type :: !Type }
  | UnboundTypeError { str :: !String }
  | ExpectedWrappedValueError { expr :: !Expr, actualType:: !Type }
  | ExpectedFunctionError { expr :: !Expr, actualType :: !Type }
  | UnexpectedLambda { expr :: !Expr, expected_type :: !Type }
  deriving (Eq)

instance Show TypeError where 
  show err = 
    case err of 
      TypeSynthesisError {expr} -> "Cannot synthesize a type for `" ++ show expr ++ "`"
      TypeCheckError { expr, expected_type, actual_type } ->
        "Expected expression `" ++ show expr ++ "` to be of type `" ++ show expected_type ++ "` but is of type `" ++ show actual_type ++ "`"
      UnboundTypeError { str } -> "Found undefined variable `" ++ str ++ "`"
      ExpectedWrappedValueError { expr, actualType } ->
        "Expected expression `" ++ show expr ++ "` to have an IO type, but is of type `" ++ show actualType ++ "`"
      ExpectedFunctionError { expr , actualType } -> "Expected expression `" ++ show expr ++ "` to have a function type, but is of type `" ++ show actualType ++ "`"
      UnexpectedLambda { expr, expected_type } -> "Expected `" ++ show expr ++ "` to be of type `" ++ show expected_type ++ "` but is a lambda"

{- 
 - Given a type environment, an expression, and a type, returns Right Expr if the 
 - Expression matches the type. Returns an error otherwise.
 -}
checkType :: TypeEnv -> Expr -> Type -> Either TypeError Expr
checkType env expr t = 
  case expr of 
    Lam str body-> 
      case t of 
        FunT argT bodyT -> do 
          checkType (insert str argT env) body bodyT;
          return expr
        o -> Left (UnexpectedLambda expr t)
    Ann body bodyT -> do 
      checkType env body bodyT;
      matchExpectedType t bodyT expr
    IOReturn body ->
      (case t of 
          IOT rt -> do 
            checkType env body rt;
            return expr
          o -> Left (TypeCheckError expr t o))
    App fnExpr argExpr ->
      case synthType env expr of 
          Right (_, exprT) -> 
            matchExpectedType t exprT expr
          Left _ -> do
            (_, argT) <- synthType env argExpr
            checkType env fnExpr (FunT argT t);
            return expr
    IOBind argExpr fnExpr -> 
       case synthType env expr of
         Right (_, exprT) -> 
            matchExpectedType t exprT expr
         Left _  -> do 
            (_, wrappedArgT) <- synthType env argExpr;
            case (wrappedArgT, t) of 
              (IOT argT, IOT outT) -> do 
                checkType env fnExpr (FunT argT t);
                return expr
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
       Left (TypeCheckError expr expected actual)



{- Given a TypeEnv and an expression, returns Right (Expr, Type)  if the expression is 
 - well typed. The returned type will be a tuple of the the expression and its type. 
 - Otherwise, an error is returned.
 -}
synthType :: TypeEnv -> Expr -> Either TypeError (Expr, Type)
synthType env expr = 
  case expr of
      Var str -> case Map.lookup str env of 
        (Just t) -> Right (expr, t)
        Nothing -> Left (UnboundTypeError str)
      Lam arg body -> Left (TypeSynthesisError expr)
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
        return (expr, annT)
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
        return (expr, IOT exprT)


