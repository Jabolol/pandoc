{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Options
  ( Parser (..),
    parse,
    intOption,
    strOption,
    enumOption,
    maybeStrOption,
    maybeEnumOption,
  )
where

import qualified Text.Read as T

newtype Parser a = Parser
  { runParser :: [String] -> Either String (a, [String])
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \args -> do
    (x, args') <- p args
    return (f x, args')

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \args -> Right (x, args)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser px = Parser $ \args -> do
    (f, args') <- pf args
    (x, args'') <- px args'
    return (f x, args'')

instance Monad Parser where
  return :: a -> Parser a
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= f = Parser $ \args -> do
    (x, args') <- p args
    runParser (f x) args'

parse :: Parser a -> [String] -> Either String a
parse p args = case runParser p args of
  Left err -> Left err
  Right (x, []) -> Right x
  Right (_, _ : _) -> Left "Unused arguments remain."

intOption :: String -> Parser Int
intOption name = Parser $ \case
  [] -> Left "Empty argument list"
  (x : xs)
    | ("-" ++ name) == x -> case xs of
        [] -> Left ("No value provided for option: " ++ name)
        (v : rest) -> case T.readMaybe v of
          Just parsed -> Right (parsed, rest)
          Nothing -> Left ("Failed to parse int: " ++ v)
    | otherwise -> Left ("Int option not found: " ++ name)

strOption :: String -> Parser String
strOption name = Parser $ \case
  [] -> Left "Empty argument list"
  (x : xs)
    | ("-" ++ name) == x -> case xs of
        [] -> Left ("No value provided for option: " ++ name)
        (v : rest) -> Right (v, rest)
    | otherwise -> Left ("String option not found: " ++ x)

enumOption :: String -> [String] -> Parser String
enumOption name options = Parser $ \case
  [] -> Left "Empty argument list"
  (x : xs)
    | ("-" ++ name) == x -> case xs of
        [] -> Left ("No value provided for option: " ++ name)
        (v : rest)
          | v `elem` options -> Right (v, rest)
          | otherwise -> Left ("Invalid value for option: " ++ name)
    | otherwise -> Right (head options, x : xs)

maybeStrOption :: String -> Parser (Maybe String)
maybeStrOption name = Parser $ \case
  [] -> Right (Nothing, [])
  (x : xs)
    | ("-" ++ name) == x -> case xs of
        [] -> Left ("No value provided for option: " ++ name)
        (v : rest) -> Right (Just v, rest)
    | otherwise -> Right (Nothing, x : xs)

maybeEnumOption :: String -> [String] -> Parser (Maybe String)
maybeEnumOption name options = Parser $ \case
  [] -> Right (Nothing, [])
  (x : xs)
    | ("-" ++ name) == x -> case xs of
        [] -> Left ("No value provided for option: " ++ name)
        (v : rest)
          | v `elem` options -> Right (Just v, rest)
          | otherwise -> Left ("Invalid value for option: " ++ name)
    | otherwise -> Right (Nothing, x : xs)
