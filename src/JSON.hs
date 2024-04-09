{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module JSON
  ( parseJSON,
  )
where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.Char as C
import qualified Data.Functor as F

data JValue
  = JString String
  | JNumber {int :: Integer, frac :: [Int], exp :: Integer}
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord)

instance Show JValue where
  show :: JValue -> String
  show (JString s) = show s
  show (JNumber s [] 0) = show s
  show (JNumber s f 0) = show s ++ "." ++ concatMap show f
  show (JNumber s [] e) = show s ++ "e" ++ show e
  show (JNumber s f e) = show s ++ "." ++ concatMap show f ++ "e" ++ show e
  show (JBool b) = show b
  show JNull = "null"
  show (JObject o) = "{" ++ showObject o ++ "}"
  show (JArray a) = "[" ++ showArray a ++ "]"

showObject :: [(String, JValue)] -> String
showObject [] = ""
showObject [(k, v)] = show k ++ ": " ++ show v
showObject ((k, v) : xs) = show k ++ ": " ++ show v ++ ", " ++ showObject xs

showArray :: [JValue] -> String
showArray [] = ""
showArray [x] = show x
showArray (x : xs) = show x ++ ", " ++ showArray xs

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

jNull :: Parser String JValue
jNull = string "null" F.$> JNull

jBool :: Parser String JValue
jBool =
  (string "true" F.$> JBool True)
    A.<|> (string "false" F.$> JBool False)

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString') -- 1
  where
    jString' = do
      optFirst <- A.optional jsonChar
      case optFirst of
        Nothing -> "" <$ char '"'
        Just first -> do
          rest <- A.many jsonChar
          _ <- char '"'
          pure (first : rest)

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

jUInt :: Parser String Integer
jUInt =
  (\d ds -> digitsToNumber 10 0 (d : ds))
    <$> digitOneToNine
    <*> digits
    A.<|> fromIntegral
      <$> digit

jInt' :: Parser String Integer
jInt' =
  signedInt
    <$> A.optional (char '-')
    <*> jUInt

signedInt :: Maybe Char -> Integer -> Integer
signedInt (Just '-') = negate
signedInt _ = id

jsonChar :: Parser String Char
jsonChar =
  string "\\\""
    F.$> '"'
    A.<|> string "\\\\"
      F.$> '\\'
    A.<|> string "\\/"
      F.$> '/'
    A.<|> string "\\b"
      F.$> '\b'
    A.<|> string "\\f"
      F.$> '\f'
    A.<|> string "\\n"
      F.$> '\n'
    A.<|> string "\\r"
      F.$> '\r'
    A.<|> string "\\t"
      F.$> '\t'
    A.<|> unicodeChar
    A.<|> matches (\c -> not (c == '\"' || c == '\\' || C.isControl c))
  where
    unicodeChar =
      C.chr . fromIntegral . digitsToNumber 16 0
        <$> (string "\\u" *> M.replicateM 4 (matches C.isHexDigit))

jFrac :: Parser String [Int]
jFrac = char '.' *> A.some digit

jExp :: Parser String Integer
jExp =
  (char 'e' A.<|> char 'E')
    *> (signedInt <$> A.optional (char '+' A.<|> char '-') <*> jUInt)

jInt :: Parser String JValue
jInt =
  JNumber
    <$> jInt'
    <*> pure []
    <*> pure 0

jIntExp :: Parser String JValue
jIntExp =
  JNumber
    <$> jInt'
    <*> pure []
    <*> jExp

jIntFrac :: Parser String JValue
jIntFrac =
  (\i f -> JNumber i f 0)
    <$> jInt'
    <*> jFrac

jIntFracExp :: Parser String JValue
jIntFracExp =
  (\ ~(JNumber i f _) e -> JNumber i f e)
    <$> jIntFrac
    <*> jExp

jNumber :: Parser String JValue
jNumber = jIntFracExp A.<|> jIntExp A.<|> jIntFrac A.<|> jInt

surroundedBy :: Parser String a -> Parser String b -> Parser String a
surroundedBy p q = q *> p <* q

separatedBy :: Parser i a -> Parser i b -> Parser i [a]
separatedBy p q = (:) <$> p <*> A.many (q *> p) A.<|> pure []

spaces :: Parser String String
spaces = A.many $ char ' ' A.<|> char '\n' A.<|> char '\t'

jArray :: Parser String JValue
jArray =
  JArray <$> (char '[' *> (jValue `separatedBy` char ',' `surroundedBy` spaces) <* char ']')

jObject :: Parser String JValue
jObject =
  JObject
    <$> (char '{' *> pair `separatedBy` char ',' `surroundedBy` spaces <* char '}')
  where
    pair =
      (\ ~(JString s) j -> (s, j))
        <$> (jString `surroundedBy` spaces)
        <* char ':'
        <*> jValue

jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
  where
    jValue' =
      jNull
        A.<|> jBool
        A.<|> jString
        A.<|> jNumber
        A.<|> jArray
        A.<|> jObject

parseJSON :: String -> Maybe JValue
parseJSON s = case parse jValue s of
  Just ("", j) -> Just j
  _ -> Nothing
