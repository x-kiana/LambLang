module DataTypes where

data Expr =
     Var String
   | Lam String Expr
   | App Expr Expr
   | StrLit String
   | UnitLit
   | Ann Expr Type
   | IOBind Expr Expr
   | IOReturn Expr
   deriving Eq

instance Show Expr where
  show (Var x) = x
  show (Lam x b) = "\\" ++ x ++ " -> " ++ show b
  show (App (App f a) b) = show (App f a) ++ " " ++ showArg b
  show (App f a) = showArg f ++ " " ++ showArg a
  show (StrLit s) = show s
  show (UnitLit) = "()"
  show (Ann e t) = show e ++ " : " ++ show t
  show (IOBind a (Ann e t)) = showArg a ++ " >>= " ++ showArg (Ann e t)
  show (IOBind a f) = show a ++ " >>= " ++ show f
  show (IOReturn e) = showArg e

showArg :: Expr -> String
showArg e
  | needsParens e = "(" ++ show e ++ ")"
  | otherwise = show e
  where
    needsParens (Var _) = False
    needsParens (Lam _ _) = True
    needsParens (App _ _) = True
    needsParens (StrLit _) = False
    needsParens (UnitLit) = False
    needsParens (Ann _ _) = True
    needsParens (IOBind _ _) = True
    needsParens (IOReturn _) = True

data Type =
    StrT
  | UnitT
  | FunT Type Type
  | IOT Type
  deriving Eq

instance Show Type where
  show (StrT) = "String"
  show (UnitT) = "Unit"
  show (FunT dom codom) = showArgT dom ++ " -> " ++ show codom
  show (IOT t) = "IO " ++ showArgT t

showArgT :: Type -> String
showArgT t
  | needsParens t = "(" ++ show t ++ ")"
  | otherwise = show t
  where
    needsParens (StrT) = False
    needsParens (UnitT) = False
    needsParens (FunT _ _) = True
    needsParens (IOT _) = True
