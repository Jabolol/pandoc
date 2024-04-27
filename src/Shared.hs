{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Shared
  ( Parser,
    Options (..),
    matches,
    char,
    digit,
    string,
    parse,
    trim,
    trimNewlines,
    escapeChar,
    toTab,
    surroundedBy,
    separatedBy,
    spaces,
    signedInt,
    digits,
    digitOneToNine,
    digitsToNumber,
    tabs,
    isSpecial,
  )
where

import qualified Control.Applicative as A
import qualified Data.Char as C

data Options = Options
  { iFile :: String,
    oFormat :: String,
    oFile :: Maybe String,
    iFormat :: Maybe String
  }
  deriving (Show)

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

trim :: String -> String
trim = replaceConsecutiveSpaces . removeNewlines

replaceConsecutiveSpaces :: String -> String
replaceConsecutiveSpaces [] = []
replaceConsecutiveSpaces (x : xs) = x : go x xs
  where
    go _ [] = []
    go prevSpace (c : cs)
      | C.isSpace prevSpace && C.isSpace c = go prevSpace cs
      | otherwise = c : go c cs

removeNewlines :: String -> String
removeNewlines = filter (/= '\n')

trimNewlines :: String -> String
trimNewlines = reverse . dropWhile (== '\n') . reverse

escapeChar :: Char -> String
escapeChar c
  | c `elem` "\"\\" = ['\\', c]
  | C.isControl c =
      '\\' : case c of
        '\b' -> ['b']
        '\f' -> ['f']
        '\n' -> ['n']
        '\r' -> ['r']
        '\t' -> ['t']
        _ ->
          let code = C.ord c
           in [ 'u',
                C.intToDigit (code `div` 16),
                C.intToDigit (code `mod` 16)
              ]
  | otherwise = [c]

toTab :: String -> String
toTab [] = []
toTab (' ' : ' ' : ' ' : ' ' : xs) = '\t' : toTab xs
toTab (x : xs) = x : toTab xs

surroundedBy :: Parser String a -> Parser String b -> Parser String a
surroundedBy p q = q *> p <* q

separatedBy :: Parser i a -> Parser i b -> Parser i [a]
separatedBy p q = (:) <$> p <*> A.many (q *> p) A.<|> pure []

spaces :: Parser String String
spaces = A.many $ char ' ' A.<|> char '\n'

tabs :: Parser String String
tabs = A.many $ char '\t' A.<|> char '\n'

digitsToNumber :: Integer -> Integer -> String -> Integer
digitsToNumber _ acc [] = acc
digitsToNumber base acc (d : ds) =
  digitsToNumber
    base
    (acc * base + toInteger (C.digitToInt d))
    ds

digitOneToNine :: Parser String Char
digitOneToNine = matches $ \c -> C.isDigit c && c /= '0'

digits :: Parser String String
digits = A.some $ matches C.isDigit

signedInt :: Maybe Char -> Integer -> Integer
signedInt (Just '-') = negate
signedInt _ = id

isSpecial :: Char -> Bool
isSpecial c = c `elem` ['*', '`', '~', '!', '[', '_', '#']
