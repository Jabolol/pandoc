{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Shared
  ( Parser,
    matches,
    char,
    digit,
    string,
    parse,
  )
where

import qualified Control.Applicative as A
import qualified Data.Char as C

newtype Parser i o = Parser {parse :: i -> Maybe (i, o)}

instance Functor (Parser i) where
  fmap :: (a -> b) -> Parser i a -> Parser i b
  fmap f parser = Parser $ fmap (fmap f) . parse parser

instance Applicative (Parser i) where
  pure :: a -> Parser i a
  pure x = Parser $ pure . (,x)

  (<*>) :: Parser i (a -> b) -> Parser i a -> Parser i b
  x <*> y = Parser $ \i -> case parse x i of
    Just (i', f) -> fmap f <$> parse y i'
    Nothing -> Nothing

instance A.Alternative (Parser i) where
  empty :: Parser i a
  empty = Parser $ const A.empty

  (<|>) :: Parser i a -> Parser i a -> Parser i a
  x <|> y = Parser $ \i -> parse x i A.<|> parse y i

instance Monad (Parser i) where
  (>>=) :: Parser i a -> (a -> Parser i b) -> Parser i b
  x >>= f = Parser $ \i -> case parse x i of
    Just (i', a) -> parse (f a) i'
    Nothing -> Nothing

matches :: (a -> Bool) -> Parser [a] a
matches p = Parser $ \case
  (x : xs) | p x -> Just (xs, x)
  _ -> Nothing

char :: Char -> Parser String Char
char c = matches (== c)

digit :: Parser String Int
digit = C.digitToInt <$> matches C.isDigit

string :: String -> Parser String String
string "" = pure ""
string (x : xs) =
  (:)
    <$> char x
    <*> string xs
