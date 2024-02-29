{-# LANGUAGE ApplicativeDo #-}

module Parse where

import Ewe
import DataTypes
import Data.Char
import Control.Applicative

parse :: String -> Either String Expr
parse s =
  case runParser exprP s of
    Nothing -> Left "Parse error: invalid LambLang expression"
    Just exp -> Right exp

varChar :: Char -> Bool
varChar = isAlpha

varP :: Parser Char String
varP = some (is varChar)

exprP :: Parser Char Expr
exprP = precedenceP [ annP, ioP, appP, ioRet ] argP

ioRet :: Parser Char Expr -> Parser Char Expr
ioRet p = (do 
  string "return"
  whitespace
  ret <- p
  pure (IOReturn ret)) <|> p

ioP :: Parser Char Expr -> Parser Char Expr
ioP p = do 
  f <- p
  args <- many (whitespace *> string ">>=" *> whitespace *> p)
  pure (mkRAssocOp IOBind f args)

argP :: Parser Char Expr
argP = foldr (<|>) failP [ Var <$> varP, parensP exprP, lamP, stringP, unitP ]

parensP :: Parser Char a -> Parser Char a
parensP p = tok '(' *> whitespace *> p <* whitespace <* tok ')'

appP :: Parser Char Expr -> Parser Char Expr
appP p = do
  f <- p 
  args <- many (whitespace *> p)
  pure (foldl App f args)

lamP :: Parser Char Expr
lamP = do
  tok '\\'
  whitespace
  x <- varP
  whitespace
  (tok '-' *> tok '>')
  whitespace
  body <- exprP
  pure (Lam x body)

annP :: Parser Char Expr -> Parser Char Expr
annP p = do
  exp <- p
  maybeT <- (do
    whitespace
    tok ':'
    whitespace
    t <- typP
    pure (Just t)) <|> pure Nothing
  pure (maybe exp (Ann exp) maybeT)

typP :: Parser Char Type
typP = precedenceP [funTP, ioTP] argTP

stringP :: Parser Char Expr
stringP = do
  tok '"'
  s <- many (is (/='"'))
  tok '"'
  pure (StrLit s)

unitP :: Parser Char Expr
unitP = do
  tok '('
  tok ')'
  pure (UnitLit)

argTP :: Parser Char Type
argTP = stringTP <|> unitTP <|> parensP typP

funTP :: Parser Char Type -> Parser Char Type
funTP p = do
  dom <- p
  whitespace
  codoms <- many (whitespace *> string "->" *> whitespace *> p)
  pure (mkRAssocOp FunT dom codoms)

ioTP :: Parser Char Type -> Parser Char Type
ioTP p = (do
  string "IO"
  whitespace1
  t <- p
  pure (IOT t)) <|> p

stringTP :: Parser Char Type
stringTP = string "String" *> pure StrT

unitTP :: Parser Char Type
unitTP = string "Unit" *> pure UnitT

precedenceP :: [Parser t a -> Parser t a] -> (Parser t a) -> Parser t a
precedenceP ps p = foldr ($) p ps

mkRAssocOp :: (a -> a -> a) -> a -> [a] -> a
mkRAssocOp f a [] = a
mkRAssocOp f a (hd : tl) = f a (mkRAssocOp f hd tl)
