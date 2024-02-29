{-# LANGUAGE GADTs, ApplicativeDo #-}
module Ewe(
  Parser,
  runParser,
  tok,
  is,
  whitespace,
  whitespace1,
  failP,
  string
) where

import Control.Applicative
import Data.Char

-- Parser t a is a computation that transforms tokens of type t into the output type a
data Parser t a where
  Fail :: Parser t a
  Pure :: a -> Parser t a
  Tok :: Eq t => t -> Parser t t
  Is :: (t -> Bool) -> Parser t t
  Many :: Parser t a -> Parser t [a]
  Choice :: Parser t a -> Parser t a -> Parser t a
  Seq :: Parser t a -> Parser t b -> Parser t b
  App :: Parser t (a -> b) -> Parser t a -> Parser t b

instance Functor (Parser t) where
  fmap f p = App (Pure f) p

instance Applicative (Parser t) where
  pure = Pure
  (<*>) = App

instance Alternative (Parser t) where
  empty = Fail
  (<|>) = Choice

-- run given parser on given list of tokens, return Nothing or Just of pair of the output of type a and remaining tokens
evalParser :: Parser t a -> [t] -> Maybe (a, [t])
evalParser Fail _ = Nothing
evalParser (Pure a) ts = Just (a, ts)
evalParser (Tok t) (hd : tl) | hd == t = Just (t, tl)
evalParser (Tok t) _ = Nothing
evalParser (Is p) (hd : tl) | p hd = Just (hd, tl)
evalParser (Is p) _ = Nothing
evalParser (Many p) ts = (evalParser p ts >>= \(hd, ts) -> evalParser (Many p) ts >>= \(tl, ts) -> Just (hd : tl, ts)) <|> Just ([], ts)
evalParser (Choice a b) ts = evalParser a ts <|> evalParser b ts
evalParser (Seq a b) ts = do
  (_, ts) <- evalParser a ts
  evalParser b ts
evalParser (App pf pa) ts = do
  (f, ts) <- evalParser pf ts
  (a, ts) <- evalParser pa ts
  pure (f a, ts)

-- run the given parser on a list of tokens, return Nothing or Just of the output
-- calls evalParser and ignores the remaining tokens
runParser :: Parser t a -> [t] -> Maybe a
runParser p ts = fst <$> evalParser p ts

-- given an input of type a, produce the parser of type Parser a a
tok :: Eq a => a -> Parser a a
tok = Tok

-- given an input of type list of a, produce a parser over list of a's
string :: Eq a => [a] -> Parser a [a]
string = foldr ((<*>) . ((:) <$>)) (pure []) . map tok

-- given a function from a to boolean as input, produce parser over a
is :: (a -> Bool) -> Parser a a
is = Is

-- parser to parse zero or more whitespaces
whitespace :: Parser Char ()
whitespace = many (is isSpace) *> pure ()

-- parser to parse one or more whitespaces
whitespace1 :: Parser Char ()
whitespace1 = some (is isSpace) *> pure ()

-- parser that always fails to parse
failP :: Parser t a
failP = Fail
