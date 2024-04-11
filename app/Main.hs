module Main (main) where

import qualified Core as C
import qualified Text.Pretty.Simple as P

main :: IO ()
main = do
  contents <- readFile "examples/syntaxe.json"
  let parsed = C.parseJSON contents
  case parsed of
    Just value -> do
      let sth = C.jsonToDocument value
      case sth of
        Left err -> putStrLn err
        Right doc -> P.pPrint doc
    _ -> error "Failed to parse JSON"
  return ()
