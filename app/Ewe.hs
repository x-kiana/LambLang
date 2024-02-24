{-# LANGUAGE ApplicativeDo #-}
module Ewe(
  Parser,
  runParser,
  is,
  tok
) where

import Control.Applicative

data Parser t a = Parser ([t] -> Maybe (a, [t]))

instance Functor (Parser t) where
  fmap f (Parser p) =
    Parser (\input -> do
      (a, output) <- p input
      return (f a, output))

instance Applicative (Parser t) where
  pure a = Parser (\input -> return (a, input))
  (Parser f) <*> (Parser a) =
    Parser (\input -> do
      (f', fOutput) <- f input
      (a', aOutput) <- a fOutput
      return (f' a', aOutput))

instance Alternative (Parser t) where
  empty = Parser (const Nothing)
  (Parser pa) <|> (Parser pb) = Parser (\input -> pa input <|> pb input)

runParser :: Parser a b -> [a] -> Maybe b
runParser (Parser p) = fmap fst . p

is :: (a -> Bool) -> Parser a a
is p =
  Parser (\input -> case input of
    (hd : tl) | p hd -> return (hd, tl)
    _ -> Nothing)

tok :: Eq a => a -> Parser a a
tok a = is (== a)

-- many :: Parser a b -> Parser a [b]
-- many p = (:) <$> p <*> many p
