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
exprP = appP

argP :: Parser Char Expr
argP = foldr (<|>) failP [ Var <$> varP, parensP exprP, lamP ]

parensP :: Parser Char a -> Parser Char a
parensP p = tok '(' *> whitespace *> p <* whitespace <* tok ')'

appP :: Parser Char Expr
appP =
  (\f -> maybe f (foldl App f))
    <$> argP
    <*> ((Just <$> many (whitespace1 *> argP)) <|> pure Nothing)

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

annP :: Parser Char Expr
annP = do
  exp <- appP
  maybeT <- (do
    whitespace
    tok ':'
    whitespace
    t <- typP
    pure (Just t)) <|> pure Nothing
  pure (maybe exp (Ann exp) maybeT)

typP :: Parser Char Type
typP = undefined

{-
exprP :: Parser Char Expr
exprP = precedenceP [ annP, appP ] argP = annP $ appP $ argP

precedenceP :: [Parser Char Expr -> Parser Char Expr] -> (Parser Char Expr) -> Parser Char Expr
precedenceP ps p = foldr ($) p ps

precP :: [(Parser Char (), Expr -> Expr -> Expr)] -> (Parser Char Expr) -> Parser Char Expr

exprP = precP [(whitespace *> tok ':'
-}
