module Main (main) where

import qualified Core as C
import qualified System.Environment as E

main :: IO ()
main = do
  args <- E.getArgs
  case args of
    [path, "json"] -> handleConversion path C.parseJSON C.jToString "JSON"
    [path, "xml"] -> handleConversion path C.parseXML C.xToString "XML"
    _ -> putStrLn "Usage: ./pandoc /path/to/file type <json/xml>"

handleConversion :: FilePath -> (String -> Maybe a) -> (a -> String) -> String -> IO ()
handleConversion path parseFn toStringFn formatName = do
  fileContents <- readFile path
  case parseFn fileContents of
    Just value -> putStrLn (toStringFn value)
    Nothing -> putStrLn $ "Failed to parse " ++ formatName
