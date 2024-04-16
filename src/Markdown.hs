{-# LANGUAGE InstanceSigs #-}

module Markdown
  ( parseMarkdown,
    mToString,
    MValue (..),
  )
where

import qualified Control.Applicative as A
import qualified Data.Char as C
import qualified Lib as L
import qualified Shared as S

data MValue
  = MMeta [(String, String)]
  | MText String
  | MParagraph [MValue]
  | MHeader Int String
  | MList Bool [MValue]
  | MCodeBlock String [MValue]
  | MQuote [MValue]
  | MLink String String
  | MImage String String
  | MBold String
  | MItalic String
  | MStrikethrough String
  | MCode String
  | MComment String
  | MRoot (MValue, MValue)
  | MBody [MValue]
  | MSection MValue [MValue]
  deriving (Eq, Ord)

mToString :: MValue -> String
mToString (MMeta m) = "---\n" ++ showMeta m ++ "\n---\n"
mToString (MText t) = t
mToString (MParagraph p) = showParagraph p
mToString (MHeader l t) = showHeader l t
mToString (MList b l) = showMList b l
mToString (MCodeBlock l c) = showCodeBlock l c
mToString (MQuote q) = showQuote q
mToString (MLink t u) = showLink t u
mToString (MImage t u) = showImage t u
mToString (MBold b) = showBold b
mToString (MItalic i) = showItalic i
mToString (MStrikethrough s) = showStrikethrough s
mToString (MCode c) = showCode c
mToString (MComment c) = "<!-- " ++ c ++ " -->"
mToString (MRoot (a, b)) = mToString a ++ "\n" ++ mToString b
mToString (MBody b) = accumulateString b
mToString (MSection (MHeader l c) b) = showHeader l c ++ "\n" ++ accumulateString b
mToString _ = "[ERR] Got something unexpected [ERR]"

instance Show MValue where
  show :: MValue -> String
  show = mToString

accumulateString :: [MValue] -> String
accumulateString = foldr (\x acc -> mToString x ++ "\n" ++ S.trimNewlines acc) ""

joinValues :: [MValue] -> String
joinValues = foldr (\x acc -> mToString x ++ S.trimNewlines acc) ""

showMeta :: [(String, String)] -> String
showMeta [] = ""
showMeta [(k, v)] = k ++ ": " ++ v
showMeta ((k, v) : xs) = k ++ ": " ++ v ++ "\n" ++ showMeta xs

showParagraph :: [MValue] -> String
showParagraph = foldr (\x acc -> mToString x ++ " " ++ acc) "\n"

showHeader :: Int -> String -> String
showHeader _ "" = ""
showHeader l t = replicate l '#' ++ " " ++ t ++ "\n"

showMList :: Bool -> [MValue] -> String
showMList b = foldr (\x acc -> (if b then "* " else "- ") ++ mToString x ++ acc) ""

showCodeBlock :: String -> [MValue] -> String
showCodeBlock l c = "```" ++ l ++ "\n" ++ joinValues c ++ "```\n"

showQuote :: [MValue] -> String
showQuote q = "> " ++ foldr (\x acc -> mToString x ++ " " ++ acc) "" q

showLink :: String -> String -> String
showLink l a = "[" ++ a ++ "](" ++ l ++ ")"

showImage :: String -> String -> String
showImage a l = "!" ++ showLink a l

showBold :: String -> String
showBold b = "**" ++ b ++ "**"

showItalic :: String -> String
showItalic i = "*" ++ i ++ "*"

showStrikethrough :: String -> String
showStrikethrough s = "~~" ++ s ++ "~~"

showCode :: String -> String
showCode c = "`" ++ c ++ "`"

mMeta :: S.Parser String MValue
mMeta = do
  _ <- S.string "---" <* L.spaces
  m <- A.many $ do
    k <- A.some (S.matches C.isAlphaNum)
    _ <- S.char ':' <* L.spaces
    v <- A.some (S.matches C.isPrint) <* L.spaces
    pure (k, v)
  _ <- L.spaces *> S.string "---" <* L.spaces
  pure $ MMeta m

mText :: S.Parser String MValue
mText = do
  t <- A.some (S.matches C.isPrint)
  pure $ MText t

-- TODO: Fix this not counting hashtags correctly
mHeader :: S.Parser String MValue
mHeader = do
  l <- A.some (S.char '#') <* L.spaces
  t <- A.some (S.matches C.isPrint) <* L.spaces
  pure $ MHeader (length l) t

mList :: S.Parser String MValue
mList = do
  b <- S.char '*' A.<|> S.char '-'
  l <- L.spaces *> A.many mValue
  pure $ MList (b == '*') l

mCodeBlock :: S.Parser String MValue
mCodeBlock = do
  _ <- S.string "```" <* L.spaces
  l <- A.some (S.matches C.isAlphaNum) <* L.spaces
  c <- A.many mValue <* L.spaces
  _ <- S.string "```"
  pure $ MCodeBlock l c

mQuote :: S.Parser String MValue
mQuote = do
  _ <- S.char '>' <* L.spaces
  q <- A.many mValue
  pure $ MQuote q

mLink :: S.Parser String MValue
mLink = do
  _ <- S.char '['
  t <- A.some (S.matches C.isPrint)
  _ <- S.char ']'
  _ <- S.char '('
  u <- A.some (S.matches C.isPrint)
  _ <- S.char ')'
  pure $ MLink t u

mImage :: S.Parser String MValue
mImage = do
  _ <- S.string "!["
  t <- A.some (S.matches C.isPrint)
  _ <- S.char ']'
  _ <- S.char '('
  u <- A.some (S.matches C.isPrint)
  _ <- S.char ')'
  pure $ MImage t u

mBold :: S.Parser String MValue
mBold = do
  _ <- S.string "**"
  b <- A.some (S.matches C.isPrint)
  _ <- S.string "**"
  pure $ MBold b

mItalic :: S.Parser String MValue
mItalic = do
  _ <- S.char '*'
  i <- A.some (S.matches C.isPrint)
  _ <- S.char '*'
  pure $ MItalic i

mStrikethrough :: S.Parser String MValue
mStrikethrough = do
  _ <- S.string "~~"
  s <- A.some (S.matches C.isPrint)
  _ <- S.string "~~"
  pure $ MStrikethrough s

mCode :: S.Parser String MValue
mCode = do
  _ <- S.char '`'
  c <- A.some (S.matches C.isPrint)
  _ <- S.char '`'
  pure $ MCode c

mComment :: S.Parser String MValue
mComment = do
  _ <- S.string "<!--"
  c <- A.some (S.matches C.isPrint)
  _ <- S.string "-->"
  pure $ MComment c

mValue :: S.Parser String MValue
mValue =
  mMeta
    A.<|> mHeader
    A.<|> mComment
    A.<|> mList
    A.<|> mCodeBlock
    A.<|> mQuote
    A.<|> mLink
    A.<|> mImage
    A.<|> mBold
    A.<|> mItalic
    A.<|> mStrikethrough
    A.<|> mCode
    A.<|> mText

parseMarkdown :: String -> Maybe MValue
parseMarkdown s = case S.parse mValue s of
  Just ("", m) -> Just m
  _ -> Nothing
