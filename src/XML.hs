{-# LANGUAGE InstanceSigs #-}

module XML
  ( parseXML,
    xToString,
    XValue (..),
  )
where

import qualified Control.Applicative as A
import qualified Data.Char as C
import qualified Lib as L
import qualified Shared as S

data XValue
  = XTag String [(String, String)] [XValue]
  | XText String
  deriving (Eq, Ord)

xToString :: XValue -> String
xToString (XTag n [] []) =
  "<"
    ++ n
    ++ ">"
    ++ "</"
    ++ n
    ++ ">"
xToString (XTag n a []) =
  "<"
    ++ n
    ++ " "
    ++ showAttributes a
    ++ ">"
    ++ "</"
    ++ n
    ++ ">"
xToString (XTag n [] c) =
  "<"
    ++ n
    ++ ">"
    ++ showChildren c
    ++ "</"
    ++ n
    ++ ">"
xToString (XTag n a c) =
  "<"
    ++ n
    ++ " "
    ++ showAttributes a
    ++ ">"
    ++ showChildren c
    ++ "</"
    ++ n
    ++ ">"
xToString (XText t) = t

instance Show XValue where
  show :: XValue -> String
  show = xToString

showAttributes :: [(String, String)] -> String
showAttributes [] = ""
showAttributes [(k, v)] = k ++ "=\"" ++ v ++ "\""
showAttributes ((k, v) : xs) = k ++ "=\"" ++ v ++ "\" " ++ showAttributes xs

showChildren :: [XValue] -> String
showChildren [] = ""
showChildren [x] = show x
showChildren (x : xs) = show x ++ showChildren xs

xTag :: S.Parser String XValue
xTag = do
  _ <- S.char '<'
  n <- A.some (S.matches C.isAlphaNum)
  a <- A.many $ do
    k <- L.spaces *> A.some (S.matches C.isAlphaNum)
    _ <- L.spaces *> S.char '='
    _ <- L.spaces *> S.char '"'
    v <- A.many (S.matches (/= '"'))
    _ <- S.char '"' <* L.spaces
    pure (k, v)
  _ <- S.char '>'
  c <- L.spaces *> A.many (xTag A.<|> xText) <* L.spaces
  _ <- S.string "</" *> S.string n <* S.char '>' <* L.spaces
  pure $ XTag n a c

xText :: S.Parser String XValue
xText = A.some (S.matches (/= '<')) >>= \t -> pure $ XText $ trim t

trim :: String -> String
trim = removeLeadingTrailingSpaces . replaceConsecutiveSpaces . removeNewlines

removeLeadingTrailingSpaces :: String -> String
removeLeadingTrailingSpaces = dropWhile C.isSpace . reverse . dropWhile C.isSpace . reverse

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

xValue :: S.Parser String XValue
xValue = xTag A.<|> xText

parseXML :: String -> Maybe XValue
parseXML s = case S.parse xValue s of
  Just ("", x) -> Just x
  _ -> Nothing
