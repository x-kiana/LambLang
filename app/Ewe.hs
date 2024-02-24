{-# LANGUAGE GADTs, ApplicativeDo #-}
module Ewe(
  Parser,
  runParser,
  tok,
  is,
  whitespace,
  whitespace1,
  failP
) where

import Control.Applicative
import Data.Char

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

evalParser :: Parser t a -> [t] -> Maybe (a, [t])
evalParser Fail _ = Nothing
evalParser (Pure a) ts = Just (a, ts)
evalParser (Tok t) (hd : tl) | hd == t = Just (t, tl)
evalParser (Tok t) _ = Nothing
evalParser (Is p) (hd : tl) | p hd = Just (hd, tl)
evalParser (Is p) _ = Nothing
evalParser (Many p) ts = (evalParser p ts >>= \(hd, ts) -> evalParser (Many p) ts >>= \(tl, ts) -> Just (hd : tl, ts)) <|> Just ([], ts)
evalParser (Choice a b) ts = evalParser a ts <|> evalParser b ts
evalParser (Seq a b) ts = evalParser a ts >>= \(_, ts) -> evalParser b ts
evalParser (App pf pa) ts = do
  (f, ts) <- evalParser pf ts
  (a, ts) <- evalParser pa ts
  pure (f a, ts)

runParser :: Parser t a -> [t] -> Maybe a
runParser p ts = fst <$> evalParser p ts

tok :: Eq a => a -> Parser a a
tok = Tok

is :: (a -> Bool) -> Parser a a
is = Is

whitespace :: Parser Char ()
whitespace = many (is isSpace) *> pure ()

whitespace1 :: Parser Char ()
whitespace1 = some (is isSpace) *> pure ()

failP :: Parser t a
failP = Fail
