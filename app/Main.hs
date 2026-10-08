module Main (main) where

import qualified Cipher
import qualified Cli
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hClose, openBinaryFile, stdin, stdout)
import System.IO.Error (catchIOError, isDoesNotExistError)

main :: IO ()
main =
  getArgs >>= \a -> case Cli.parse a of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right cli ->
      if Cli.help cli
        then putStrLn Cli.cliHelp
        else
          if Cli.version cli
            then
              putStrLn Cli.appVersion
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
                        (Nothing, Nothing) -> Cipher.password $ Cli.encrypt cli
                        (Just pw, _) -> pure pw
                        (Nothing, Just kf) -> readFile kf
                        >>= \password ->
                          ( if Cli.encrypt cli
                              then
                                (getRandomBytes Cipher.saltLength :: IO ByteString) >>= \rb -> BS.hPut output rb >> pure rb
                              else BS.hGet input Cipher.saltLength
                          )
                            >>= \salt ->
                              ( if Cli.encrypt cli
                                  then
                                    (getRandomBytes Cipher.nonceLength :: IO ByteString) >>= \rb -> BS.hPut output rb >> pure rb
                                  else
                                    BS.hGet input Cipher.nonceLength
                              )
                                >>= \nonceBase ->
                                  Cipher.hashPassword password salt
                                    >>= \key ->
                                      hClose input >> hClose output
