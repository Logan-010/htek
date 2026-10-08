module Main (main) where

import qualified Cli
import qualified Crypto
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hClose, openBinaryFile, stdin, stdout)
import System.IO.Error (catchIOError, isDoesNotExistError)

main :: IO ()
main =
  getArgs >>= \a -> case Cli.parse a of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right cli ->
      if Cli.help cli
        then putStrLn Cli.cli_help
        else
          if Cli.version cli
            then
              putStrLn Cli.app_version
            else
              case Cli.input cli of
                Just f -> openBinaryFile f ReadMode
                Nothing -> pure stdin
                >>= \input ->
                  case Cli.output cli of
                    Just f ->
                      if Cli.force cli
                        then openBinaryFile f WriteMode
                        else
                          catchIOError
                            ( openBinaryFile f ReadMode >>= \h ->
                                hClose h
                                  >> ioError (userError "Output file already exists (use --force)")
                            )
                            ( \e ->
                                if isDoesNotExistError e
                                  then openBinaryFile f WriteMode
                                  else ioError e
                            )
                    Nothing -> pure stdout
                    >>= \output ->
                      case (Cli.password cli, Cli.key_file cli) of
                        (Nothing, Nothing) -> Crypto.password $ Cli.encrypt cli
                        (Just pw, _) -> pure pw
                        (Nothing, Just kf) -> readFile kf
                        >>= \password ->
                          hClose input >> hClose output
