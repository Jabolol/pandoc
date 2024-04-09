{-# LANGUAGE InstanceSigs #-}

module JSON
  ( parseJSON,
    jToString,
  )
where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.Char as C
import qualified Data.Functor as F
import qualified Lib as L
import qualified Shared as S

data JValue
  = JString String
  | JNumber {int :: Integer, frac :: [Int], exp :: Integer}
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord)

jToString :: JValue -> String
jToString (JString s) = s
jToString (JNumber s [] 0) = show s
jToString (JNumber s f 0) = show s ++ "." ++ concatMap show f
jToString (JNumber s [] e) = show s ++ "e" ++ show e
jToString (JNumber s f e) = show s ++ "." ++ concatMap show f ++ "e" ++ show e
jToString (JBool b) = show b
jToString JNull = "null"
jToString (JObject o) = "{" ++ showObject o ++ "}"
jToString (JArray a) = "[" ++ showArray a ++ "]"

instance Show JValue where
  show :: JValue -> String
  show = jToString

showObject :: [(String, JValue)] -> String
showObject [] = ""
showObject [(k, v)] = show k ++ ": " ++ show v
showObject ((k, v) : xs) = show k ++ ": " ++ show v ++ ", " ++ showObject xs

showArray :: [JValue] -> String
showArray [] = ""
showArray [x] = show x
showArray (x : xs) = show x ++ ", " ++ showArray xs

jNull :: S.Parser String JValue
jNull = S.string "null" F.$> JNull

jBool :: S.Parser String JValue
jBool =
  (S.string "true" F.$> JBool True)
    A.<|> (S.string "false" F.$> JBool False)

jString :: S.Parser String JValue
jString = JString <$> (S.char '"' *> jString')
  where
    jString' = do
      optFirst <- A.optional jsonChar
      case optFirst of
        Nothing -> "" <$ S.char '"'
        Just first -> do
          rest <- A.many jsonChar
          _ <- S.char '"'
          pure (first : rest)

jUInt :: S.Parser String Integer
jUInt =
  (\d ds -> L.digitsToNumber 10 0 (d : ds))
    <$> L.digitOneToNine
    <*> L.digits
    A.<|> fromIntegral
      <$> S.digit

jIntNegative :: S.Parser String Integer
jIntNegative =
  L.signedInt
    <$> A.optional (S.char '-')
    <*> jUInt

jsonChar :: S.Parser String Char
jsonChar =
  S.string "\\\""
    F.$> '"'
    A.<|> S.string "\\\\"
      F.$> '\\'
    A.<|> S.string "\\/"
      F.$> '/'
    A.<|> S.string "\\b"
      F.$> '\b'
    A.<|> S.string "\\f"
      F.$> '\f'
    A.<|> S.string "\\n"
      F.$> '\n'
    A.<|> S.string "\\r"
      F.$> '\r'
    A.<|> S.string "\\t"
      F.$> '\t'
    A.<|> unicodeChar
    A.<|> S.matches
      ( \c ->
          not (c == '\"' || c == '\\' || C.isControl c)
      )
  where
    unicodeChar =
      C.chr . fromIntegral . L.digitsToNumber 16 0
        <$> (S.string "\\u" *> M.replicateM 4 (S.matches C.isHexDigit))

jFrac :: S.Parser String [Int]
jFrac = S.char '.' *> A.some S.digit

jExp :: S.Parser String Integer
jExp =
  (S.char 'e' A.<|> S.char 'E')
    *> (L.signedInt <$> A.optional (S.char '+' A.<|> S.char '-') <*> jUInt)

jInt :: S.Parser String JValue
jInt =
  JNumber
    <$> jIntNegative
    <*> pure []
    <*> pure 0

jIntExp :: S.Parser String JValue
jIntExp =
  JNumber
    <$> jIntNegative
    <*> pure []
    <*> jExp

jIntFrac :: S.Parser String JValue
jIntFrac =
  (\i f -> JNumber i f 0)
    <$> jIntNegative
    <*> jFrac

jIntFracExp :: S.Parser String JValue
jIntFracExp =
  (\ ~(JNumber i f _) e -> JNumber i f e)
    <$> jIntFrac
    <*> jExp

jNumber :: S.Parser String JValue
jNumber = jIntFracExp A.<|> jIntExp A.<|> jIntFrac A.<|> jInt

jArray :: S.Parser String JValue
jArray =
  JArray
    <$> ( S.char '['
            *> (jValue `L.separatedBy` S.char ',' `L.surroundedBy` L.spaces)
            <* S.char ']'
        )

jObject :: S.Parser String JValue
jObject =
  JObject
    <$> ( S.char '{'
            *> pair
              `L.separatedBy` S.char ','
              `L.surroundedBy` L.spaces
            <* S.char '}'
        )
  where
    pair =
      (\ ~(JString s) j -> (s, j))
        <$> (jString `L.surroundedBy` L.spaces)
        <* S.char ':'
        <*> jValue

jValue :: S.Parser String JValue
jValue = jValue' `L.surroundedBy` L.spaces
  where
    jValue' =
      jNull
        A.<|> jBool
        A.<|> jString
        A.<|> jNumber
        A.<|> jArray
        A.<|> jObject

parseJSON :: String -> Maybe JValue
parseJSON s = case S.parse jValue s of
  Just ("", j) -> Just j
  _ -> Nothing
