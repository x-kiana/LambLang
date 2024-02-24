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
  (tok 'λ' <|> tok '\\')
  whitespace
  x <- varP
  whitespace
  (tok '-' *> tok '>')
  whitespace
  body <- exprP
  pure (Lam x body)
