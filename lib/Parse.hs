{-# LANGUAGE ApplicativeDo #-}

module Parse where

import Ewe
import DataTypes
import Data.Char
import Control.Applicative

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
  pure (buildIOBind f args)
  where
    buildIOBind f args =
      case args of 
        [] -> f 
        (h:t) -> IOBind f (buildIOBind h t)

argP :: Parser Char Expr
argP = foldr (<|>) failP [ Var <$> varP, parensP exprP, lamP ]

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
typP = stringTP

stringTP :: Parser Char Type
stringTP = string "String" *> pure StrT

precedenceP :: [Parser t a -> Parser t a] -> (Parser t a) -> Parser t a
precedenceP ps p = foldr ($) p ps
{-
precP :: [(Parser Char (), Expr -> Expr -> Expr)] -> (Parser Char Expr) -> Parser Char Expr

exprP = precP [(whitespace *> tok ':'
-}

