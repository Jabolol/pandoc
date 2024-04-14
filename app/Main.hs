module Main (main) where

import qualified Core as C
import qualified System.Environment as E

main :: IO ()
main = do
  args <- E.getArgs
  case args of
    [inputPath, outputPath] -> do
      contents <- readFile inputPath
      let parsed = C.parseXML contents
      case parsed of
        Just value -> do
          let sth = C.xmlToDocument value
          case sth of
            Left err -> putStrLn err
            Right doc -> do
              writeFile outputPath $ C.mToString $ C.documentToMarkdown doc
              putStrLn $ "Conversion successful. Output written to " ++ outputPath
        _ -> error "Failed to parse XML"
    _ -> error "Usage: programName inputPath outputPath"
