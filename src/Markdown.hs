{-# LANGUAGE InstanceSigs #-}

module Markdown
  ( parseMarkdown,
    mToString,
    updateHeaders,
    MValue (..),
  )
where

import qualified Control.Applicative as A
import qualified Data.Char as C
import qualified Shared as S

data MValue
  = MMeta [(String, String)]
  | MText Bool String
  | MParagraph [MValue]
  | MHeader Int String
  | MList [MValue]
  | MCodeBlock [MValue]
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
mToString (MText _ t) = t
mToString (MParagraph p) = showParagraph p ++ "\n"
mToString (MHeader l t) = showHeader l t
mToString (MList l) = showMList l
mToString (MCodeBlock c) = showCodeBlock c
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
mToString (MSection (MHeader _ "") b) = accumulateString b
mToString (MSection (MHeader l c) b) = showHeader l c ++ "\n" ++ accumulateString b
mToString _ = ""

instance Show MValue where
  show :: MValue -> String
  show = mToString

accumulateString :: [MValue] -> String
accumulateString = foldr (\x acc -> mToString x ++ "\n" ++ S.trimNewlines acc) ""

joinValues :: [MValue] -> String
joinValues [] = ""
joinValues [x] = mToString x
joinValues (x : xs) = mToString x ++ joinValues xs

showMeta :: [(String, String)] -> String
showMeta [] = ""
showMeta [(k, v)] = k ++ ": " ++ v
showMeta ((k, v) : xs) = k ++ ": " ++ v ++ "\n" ++ showMeta xs

showParagraph :: [MValue] -> String
showParagraph [] = ""
showParagraph [x] = mToString x
showParagraph (x : xs) = mToString x ++ showParagraph xs

showHeader :: Int -> String -> String
showHeader _ "" = ""
showHeader l t = replicate l '#' ++ " " ++ t ++ "\n"

showMList :: [MValue] -> String
showMList =
  foldr
    (\x acc -> "- " ++ mToString x ++ acc)
    ""

showCodeBlock :: [MValue] -> String
showCodeBlock c = "```\n" ++ joinValues c ++ "```\n"

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

updateHeaders :: MValue -> MValue
updateHeaders = go 1
  where
    go :: Int -> MValue -> MValue
    go lvl (MParagraph p) = MParagraph (map (go lvl) p)
    go lvl (MHeader _ t) = MHeader lvl t
    go lvl (MList l) = MList (map (go lvl) l)
    go lvl (MCodeBlock c) = MCodeBlock (map (go lvl) c)
    go lvl (MQuote q) = MQuote (map (go lvl) q)
    go lvl (MRoot (a, b)) = MRoot (go lvl a, go lvl b)
    go lvl (MBody b) = MBody (map (go lvl) b)
    go lvl (MSection header body) =
      let updatedHeader = case header of
            MHeader _ _ -> MHeader lvl (headerTitle header)
            _ -> header
       in MSection updatedHeader (map (go (lvl + 1)) body)
    go _ x = x

    headerTitle :: MValue -> String
    headerTitle (MHeader _ title) = title
    headerTitle _ = ""

mMeta :: S.Parser String MValue
mMeta = do
  _ <- S.string "---" <* S.spaces
  m <- A.many $ do
    k <- A.some (S.matches C.isAlphaNum)
    _ <- S.char ':' <* S.spaces
    v <- A.some (S.matches C.isPrint) <* S.spaces
    pure (k, v)
  _ <- S.spaces *> S.string "---" <* S.spaces
  pure $ MMeta m

mText :: S.Parser String MValue
mText = do
  t <- A.some (S.matches $ \c -> C.isPrint c && not (S.isSpecial c))
  z <- S.spaces
  pure $ MText (not $ null z) t

mParagraph :: S.Parser String MValue
mParagraph = do
  p <- A.some (mText A.<|> mInner)
  pure $ MParagraph p

mHeader :: S.Parser String MValue
mHeader = do
  l <- A.some (S.char '#') <* S.spaces
  t <- A.some (S.matches C.isPrint) <* S.spaces
  pure $ MHeader (length l) t

mList :: S.Parser String MValue
mList = do
  l <- S.spaces *> A.some mItem
  pure $ MList l

mCodeBlock :: S.Parser String MValue
mCodeBlock = do
  _ <- S.string "```" <* S.spaces
  c <- A.some mText
  _ <- S.string "```"
  pure $ MCodeBlock c

mQuote :: S.Parser String MValue
mQuote = do
  _ <- S.char '>' <* S.spaces
  q <- A.many (mValue <* S.spaces)
  pure $ MQuote q

mLink :: S.Parser String MValue
mLink = do
  _ <- S.char '['
  t <- A.some (S.matches $ \c -> C.isPrint c && c /= ']')
  _ <- S.char ']'
  _ <- S.char '('
  u <- A.some (S.matches $ \c -> C.isPrint c && c /= ')')
  _ <- S.char ')'
  pure $ MLink t u

mImage :: S.Parser String MValue
mImage = do
  _ <- S.string "!["
  t <- A.some (S.matches $ \c -> C.isPrint c && c /= ']')
  _ <- S.char ']'
  _ <- S.char '('
  u <- A.some (S.matches $ \c -> C.isPrint c && c /= ')')
  _ <- S.char ')'
  pure $ MImage t u

mBold :: S.Parser String MValue
mBold = do
  _ <- S.string "**"
  b <- A.some (S.matches $ \c -> C.isPrint c && c /= '*')
  _ <- S.string "**"
  pure $ MBold b

mItalic :: S.Parser String MValue
mItalic = do
  _ <- S.char '_'
  i <- A.some (S.matches $ \c -> C.isPrint c && c /= '_')
  _ <- S.char '_'
  pure $ MItalic i

mItalic' :: S.Parser String MValue
mItalic' = do
  _ <- S.char '*'
  i <- A.some (S.matches $ \c -> C.isPrint c && c /= '*')
  _ <- S.char '*'
  pure $ MItalic i

mStrikethrough :: S.Parser String MValue
mStrikethrough = do
  _ <- S.string "~~"
  s <- A.some (S.matches $ \c -> C.isPrint c && c /= '~')
  _ <- S.string "~~"
  pure $ MStrikethrough s

mCode :: S.Parser String MValue
mCode = do
  _ <- S.char '`'
  c <- A.some (S.matches $ \c -> C.isPrint c && c /= '`')
  _ <- S.char '`'
  pure $ MCode c

mComment :: S.Parser String MValue
mComment = do
  _ <- S.string "<!--"
  c <- A.some (S.matches $ \c -> C.isPrint c && c /= '-')
  _ <- S.string "-->"
  pure $ MComment c

mItem :: S.Parser String MValue
mItem = do
  _ <- S.char '-'
  t <- S.spaces *> A.some (S.matches C.isPrint) <* S.spaces
  pure $ MText False t

mSection :: S.Parser String MValue
mSection = do
  h <- mHeader
  p <- A.some (mInner A.<|> mParagraph)
  pure $ MSection h p

mInner :: S.Parser String MValue
mInner =
  mComment
    A.<|> mList
    A.<|> mCodeBlock
    A.<|> mQuote
    A.<|> mLink
    A.<|> mImage
    A.<|> mBold
    A.<|> mItalic
    A.<|> mItalic'
    A.<|> mStrikethrough
    A.<|> mCode

mValue :: S.Parser String MValue
mValue =
  mMeta
    A.<|> mSection
    A.<|> mParagraph

parseMarkdown :: String -> Maybe MValue
parseMarkdown s = case loop s [] of
  Just m -> Just $ MBody $ reverse m
  _ -> Nothing

loop :: String -> [MValue] -> Maybe [MValue]
loop "" acc = Just acc
loop input acc =
  case S.parse mValue input of
    Just (rest, m) -> loop rest (m : acc)
    _ -> Nothing
