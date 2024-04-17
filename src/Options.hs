{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Options
  ( Parser (..),
    parse,
    intOption,
    strOption,
    enumOption,
    toMaybe,
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
intOption name = Parser $ \args -> do
  let (value, rest) = extractOption name args
  case value of
    Just v -> case T.readMaybe v of
      Just parsed -> Right (parsed, rest)
      Nothing -> Left ("Failed to parse int: " ++ v)
    Nothing -> Left ("No value provided for option: " ++ name)

strOption :: String -> Parser String
strOption name = Parser $ \args -> do
  let (value, rest) = extractOption name args
  case value of
    Just v -> Right (v, rest)
    Nothing -> Left ("No value provided for option: " ++ name)

enumOption :: String -> [String] -> Parser String
enumOption name options = Parser $ \args -> do
  let (value, rest) = extractOption name args
  case value of
    Just v ->
      if v `elem` options
        then Right (v, rest)
        else Left ("Invalid value for option: " ++ name)
    Nothing -> Right (head options, rest)

toMaybe :: Parser a -> Parser (Maybe a)
toMaybe (Parser p) = Parser $ \case
  [] -> Right (Nothing, [])
  xs -> case runParser (Just <$> Parser p) xs of
    Left _ -> Right (Nothing, xs)
    Right (Just a, rest) -> Right (Just a, rest)
    _ -> Right (Nothing, xs)

extractOption :: String -> [String] -> (Maybe String, [String])
extractOption name args = case break (== ("-" ++ name)) args of
  (_, []) -> (Nothing, args)
  (before, _ : after) -> (Just (head after), before ++ tail after)
