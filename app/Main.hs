module Main (main) where

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
    <*> O.enumOption "f" ["xml", "json"]
    <*> O.maybeStrOption "o"
    <*> O.maybeEnumOption "e" ["xml", "json", "markdown"]

run :: S.Options -> IO ()
run = undefined

main :: IO ()
main =
  E.getArgs >>= \args ->
    case O.parse options args of
      Left err ->
        I.hPutStrLn F.stderr err
          >> X.exitWith (X.ExitFailure 84)
      Right opts -> run opts
