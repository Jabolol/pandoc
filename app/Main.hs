module Main (main) where

import qualified Control.Applicative as A
import qualified Core as C
import qualified Data.Maybe as Y
import qualified GHC.IO.Handle.FD as F
import qualified Options as O
import qualified Shared as S
import qualified System.Environment as E
import qualified System.Exit as X
import qualified System.IO as I

options :: O.Parser S.Options
options =
  S.Options
    <$> O.strOption "i"
    <*> O.enumOption "f" ["xml", "json", "markdown"]
    <*> O.toMaybe (O.strOption "o")
    <*> O.toMaybe (O.enumOption "e" ["xml", "json"])

run :: S.Options -> IO ()
run opts = do
  let to = S.oFormat opts
  content <- readFile $ S.iFile opts
  let from = Y.fromMaybe "none" $ S.iFormat opts A.<|> C.try content
  let result = C.flow from content to

  case result of
    Left err -> handleError err
    Right x -> handleSuccess x
  where
    handleError err =
      I.hPutStrLn F.stderr err
        >> X.exitWith (X.ExitFailure 84)
    handleSuccess = maybe putStrLn writeFile (S.oFile opts)

main :: IO ()
main =
  E.getArgs >>= \args ->
    case O.parse options args of
      Left err ->
        I.hPutStrLn F.stderr err
          >> X.exitWith (X.ExitFailure 84)
      Right opts -> run opts
