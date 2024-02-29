{-# LANGUAGE ApplicativeDo #-}

module Parse where

import Ewe
import DataTypes
import Data.Char
import Control.Applicative

-- wrapper function over the runParser function imported from Ewe
-- given a string, calls runParser exprP on the string, returns Either Right Expr if valid or an error
parse :: String -> Either String Expr
parse s =
  case runParser exprP s of
    Nothing -> Left "Parse error: invalid LambLang expression"
    Just exp -> Right exp

-- return true if the given char is a valid variable character
varChar :: Char -> Bool
varChar = isAlpha

-- parse a string which is a valid variable
varP :: Parser Char String
varP = some (is varChar)

-- parse an expression
exprP :: Parser Char Expr
exprP = precedenceP [ annP, ioP, appP, ioRet ] argP

-- parse an IOReturn Expr
ioRet :: Parser Char Expr -> Parser Char Expr
ioRet p = (do 
  string "return"
  whitespace
  ret <- p
  pure (IOReturn ret)) <|> p

-- parse an IOBind Expr
ioP :: Parser Char Expr -> Parser Char Expr
ioP p = do 
  f <- p
  args <- many (whitespace *> string ">>=" *> whitespace *> p)
  pure (mkRAssocOp IOBind f args)

-- parse an Expr that is either a variable, an expression in parentheses, anonymous function, string, or unit
argP :: Parser Char Expr
argP = foldr (<|>) failP [ Var <$> varP, parensP exprP, lamP, stringP, unitP ]

-- given a parser, parse parentheses around it
parensP :: Parser Char a -> Parser Char a
parensP p = tok '(' *> whitespace *> p <* whitespace <* tok ')'

-- parse function application
appP :: Parser Char Expr -> Parser Char Expr
appP p = do
  f <- p 
  args <- many (whitespace *> p)
  pure (foldl App f args)

-- parse anonymous function
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

-- parse annotation expression
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

-- parse a valid string character
stringCharP :: Parser Char Char
stringCharP = (is (\c -> (c /= '"') && (c /= '\\'))) <|> (tok '\\' *> tok 'n' *> pure '\n') <|> (tok '\\' *> is (const True))

-- parse a string literal
stringP :: Parser Char Expr
stringP = do
  tok '"'
  s <- many (stringCharP)
  tok '"'
  pure (StrLit s)

-- parse a unit literal
unitP :: Parser Char Expr
unitP = do
  string "()"
  pure (UnitLit)

-- parse a type
typP :: Parser Char Type
typP = precedenceP [funTP, ioTP] argTP

-- parse a Type that is either a string, unit, or a type in parentheses
argTP :: Parser Char Type
argTP = stringTP <|> unitTP <|> parensP typP

-- parse a function type
funTP :: Parser Char Type -> Parser Char Type
funTP p = do
  dom <- p
  whitespace
  codoms <- many (whitespace *> string "->" *> whitespace *> p)
  pure (mkRAssocOp FunT dom codoms)

-- parse an IO type
ioTP :: Parser Char Type -> Parser Char Type
ioTP p = (do
  string "IO"
  whitespace1
  t <- p
  pure (IOT t)) <|> p

-- parse a string type
stringTP :: Parser Char Type
stringTP = string "String" *> pure StrT

-- parse a unit type
unitTP :: Parser Char Type
unitTP = string "Unit" *> pure UnitT

-- takes a list of precendence parsers and a base parser, and returns
-- a parser where the first precedence parser in the list has lowest precedence
-- and the last one has highest.
-- precedenceP [pp1, ..., ppn] pb = pp1 $ ... $ ppn $ pb
precedenceP :: [Parser t a -> Parser t a] -> (Parser t a) -> Parser t a
precedenceP ps p = foldr ($) p ps

-- make a right associative operator
-- mkRAssocOp op a [b1, ..., bn] = a `op` (b1 `op` ... `op` bn)
mkRAssocOp :: (a -> a -> a) -> a -> [a] -> a
mkRAssocOp f a [] = a
mkRAssocOp f a (hd : tl) = f a (mkRAssocOp f hd tl)
