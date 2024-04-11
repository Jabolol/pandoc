module Main (main) where

import qualified Core as C
import qualified Text.Pretty.Simple as P

main :: IO ()
main = do
  contents <- readFile "examples/syntaxe.xml"
  let parsed = C.parseXML contents
  case parsed of
    Just value -> do
      let sth = C.xmlToDocument value
      case sth of
        Left err -> putStrLn err
        Right doc -> P.pPrint doc
    _ -> error "Failed to parse XML"
  return ()
