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

data Type =
     StrT
   | UnitT
   | FunT Type Type
   | IOT Type