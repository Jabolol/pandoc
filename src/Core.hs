module Core
  ( flow,
    try,
  )
where

import qualified Data.Either as E
import qualified Data.List as L
import qualified Document as D

flow :: String -> String -> String -> Either String String
flow f c t = D.toDocument f c >>= D.fromDocument t

try :: String -> Maybe String
try c = L.find (\f -> E.isRight $ D.toDocument f c) ["xml", "json"]
