{-# LANGUAGE InstanceSigs #-}

module XML
  ( parseXML,
    xToString,
    XValue (..),
  )
where

import qualified Control.Applicative as A
import qualified Data.Char as C
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
    k <- S.spaces *> A.some (S.matches C.isAlphaNum)
    _ <- S.spaces *> S.char '='
    _ <- S.spaces *> S.char '"'
    v <- A.many (S.matches (/= '"'))
    _ <- S.char '"' <* S.spaces
    pure (k, v)
  _ <- S.char '>'
  c <- S.tabs *> A.many (xTag A.<|> xText) <* S.tabs
  _ <- S.string "</" *> S.string n <* S.char '>' <* S.tabs
  pure $ XTag n a c

xText :: S.Parser String XValue
xText = XText <$> A.some (S.matches (/= '<'))

xValue :: S.Parser String XValue
xValue = xTag A.<|> xText

parseXML :: String -> Maybe XValue
parseXML s = case S.parse xValue $ S.toTab s of
  Just ("", x) -> Just x
  _ -> Nothing
