module Lib
  ( surroundedBy,
    separatedBy,
    spaces,
    signedInt,
    digits,
    digitOneToNine,
    digitsToNumber,
  )
where

import qualified Control.Applicative as A
import qualified Data.Char as C
import qualified Shared as S

surroundedBy :: S.Parser String a -> S.Parser String b -> S.Parser String a
surroundedBy p q = q *> p <* q

separatedBy :: S.Parser i a -> S.Parser i b -> S.Parser i [a]
separatedBy p q = (:) <$> p <*> A.many (q *> p) A.<|> pure []

spaces :: S.Parser String String
spaces = A.many $ S.char ' ' A.<|> S.char '\n' A.<|> S.char '\t'

digitsToNumber :: Integer -> Integer -> String -> Integer
digitsToNumber _ acc [] = acc
digitsToNumber base acc (d : ds) =
  digitsToNumber
    base
    (acc * base + toInteger (C.digitToInt d))
    ds

digitOneToNine :: S.Parser String Char
digitOneToNine = S.matches $ \c -> C.isDigit c && c /= '0'

digits :: S.Parser String String
digits = A.some $ S.matches C.isDigit

signedInt :: Maybe Char -> Integer -> Integer
signedInt (Just '-') = negate
signedInt _ = id
