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
  | MCodeBlock String String
  | MQuote [MValue]
  | MLink String String
  | MImage String String
  | MBold [MValue]
  | MItalic [MValue]
  | MStrikethrough [MValue]
  | MCode String
  | MComment String
  deriving (Eq, Ord)

mToString :: MValue -> String
mToString (MMeta m) = "---\n" ++ showMeta m ++ "\n---"
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

instance Show MValue where
  show :: MValue -> String
  show = mToString

showMeta :: [(String, String)] -> String
showMeta [] = ""
showMeta [(k, v)] = k ++ ": " ++ v
showMeta ((k, v) : xs) = k ++ ": " ++ v ++ "\n" ++ showMeta xs

showParagraph :: [MValue] -> String
showParagraph [] = ""
showParagraph [x] = show x
showParagraph (x : xs) = show x ++ " " ++ showParagraph xs

showHeader :: Int -> String -> String
showHeader l t = replicate l '#' ++ " " ++ t

showMList :: Bool -> [MValue] -> String
showMList _ [] = ""
showMList True [x] = "* " ++ show x
showMList False [x] = "- " ++ show x
showMList True (x : xs) = "* " ++ show x ++ "\n" ++ showMList True xs
showMList False (x : xs) = "- " ++ show x ++ "\n" ++ showMList False xs

showCodeBlock :: String -> String -> String
showCodeBlock l c = "```" ++ l ++ "\n" ++ c ++ "\n```"

showQuote :: [MValue] -> String
showQuote q = "> " ++ showParagraph q

showLink :: String -> String -> String
showLink t u = "[" ++ t ++ "](" ++ u ++ ")"

showImage :: String -> String -> String
showImage t u = "!" ++ showLink t u

showBold :: [MValue] -> String
showBold b = "**" ++ showParagraph b ++ "**"

showItalic :: [MValue] -> String
showItalic i = "*" ++ showParagraph i ++ "*"

showStrikethrough :: [MValue] -> String
showStrikethrough s = "~~" ++ showParagraph s ++ "~~"

showCode :: String -> String
showCode c = "`" ++ c ++ "`"

mMeta :: S.Parser String MValue
mMeta = do
  _ <- S.string "---"
  _ <- L.spaces
  m <- A.many $ do
    k <- A.some (S.matches C.isAlphaNum)
    _ <- S.char ':'
    _ <- L.spaces
    v <- A.some (S.matches C.isPrint)
    _ <- L.spaces
    pure (k, v)
  _ <- L.spaces
  _ <- S.string "---"
  _ <- L.spaces
  pure $ MMeta m

mText :: S.Parser String MValue
mText = do
  t <- A.some (S.matches C.isPrint)
  pure $ MText t

mHeader :: S.Parser String MValue
mHeader = do
  l <- A.some (S.char '#')
  _ <- L.spaces
  t <- A.some (S.matches C.isPrint)
  _ <- L.spaces
  pure $ MHeader (length l) t

mList :: S.Parser String MValue
mList = do
  b <- S.char '*' A.<|> S.char '-'
  _ <- L.spaces
  l <- A.many mValue
  pure $ MList (b == '*') l

mCodeBlock :: S.Parser String MValue
mCodeBlock = do
  _ <- S.string "```"
  _ <- L.spaces
  l <- A.some (S.matches C.isAlphaNum)
  _ <- L.spaces
  c <- A.some (S.matches C.isPrint)
  _ <- L.spaces
  _ <- S.string "```"
  pure $ MCodeBlock l c

mQuote :: S.Parser String MValue
mQuote = do
  _ <- S.char '>'
  _ <- L.spaces
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
  b <- A.many mValue
  _ <- S.string "**"
  pure $ MBold b

mItalic :: S.Parser String MValue
mItalic = do
  _ <- S.char '*'
  i <- A.many mValue
  _ <- S.char '*'
  pure $ MItalic i

mStrikethrough :: S.Parser String MValue
mStrikethrough = do
  _ <- S.string "~~"
  s <- A.many mValue
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
