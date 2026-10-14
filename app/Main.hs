{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Cipher
import qualified Cli
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import qualified Data.Text.Lazy as T
import Data.Time.Clock (diffUTCTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hFileSize, hFlush, hPutStrLn, openBinaryFile, stderr, stdin, stdout)
import System.IO.Error (catchIOError, ioeGetErrorString, isDoesNotExistError)
import System.ProgressBar
import Text.Printf (printf)

speed :: Label s
speed = Label $ \p t ->
  let elapsed =
        realToFrac $
          diffUTCTime (timingLastUpdate t) (timingStart t)
      rate :: Double
      rate =
        if elapsed > 0
          then fromIntegral (progressDone p) / elapsed
          else 0
   in T.pack (printf "%.1f items/s" rate)

style :: Style ()
style =
  defStyle
    { stylePrefix = msg "Processing",
      stylePostfix =
        percentage
          <> msg " | "
          <> exact
          <> msg " | "
          <> speed
          <> msg " | ETA "
          <> remainingTime renderDuration "...",
      styleWidth = TerminalWidth 100,
      styleOnComplete = WriteNewline
    }

main :: IO ()
main =
  catchIOError
    ( do
        args <- getArgs
        cli <- either (ioError . userError . ("Parse error: " ++)) pure (Cli.parse args)
        if Cli.help cli
          then putStrLn Cli.cliHelp
          else
            if Cli.version cli
              then putStrLn Cli.appVersion
              else
                bracket
                  ( case Cli.input cli of
                      Just path -> openBinaryFile path ReadMode
                      Nothing -> pure stdin
                  )
                  ( \input -> case Cli.input cli of
                      Just _ -> hClose input
                      Nothing -> pure ()
                  )
                  ( \input ->
                      bracket
                        ( case Cli.output cli of
                            Nothing -> pure stdout
                            Just path ->
                              if Cli.force cli
                                then openBinaryFile path WriteMode
                                else
                                  catchIOError
                                    ( do
                                        handle <- openBinaryFile path ReadMode
                                        hClose handle
                                        ioError (userError "Output file already exists (use --force)")
                                    )
                                    ( \err ->
                                        if isDoesNotExistError err
                                          then openBinaryFile path WriteMode
                                          else ioError err
                                    )
                        )
                        ( \output -> case Cli.output cli of
                            Just _ -> hClose output
                            Nothing -> hFlush output
                        )
                        ( \output -> do
                            password <- case (Cli.password cli, Cli.key_file cli) of
                              (Just pw, _) -> pure pw
                              (Nothing, Just path) -> readFile path
                              (Nothing, Nothing) -> Cipher.password (Cli.encrypt cli)
                            len <- if Cli.quiet cli then pure Nothing else traverse (const $ hFileSize input) $ Cli.input cli
                            pb <- traverse ((\total -> newProgressBar style 10 $ Progress 0 total ()) . fromIntegral) len
                            salt <-
                              if Cli.encrypt cli
                                then do
                                  bytes <- getRandomBytes Cipher.saltLength
                                  BS.hPut output bytes
                                  pure bytes
                                else BS.hGet input Cipher.saltLength
                            unless (BS.length salt == Cipher.saltLength) $
                              ioError (userError "Truncated salt")
                            nonceBase <-
                              if Cli.encrypt cli
                                then do
                                  bytes <- getRandomBytes Cipher.nonceLength
                                  BS.hPut output bytes
                                  pure bytes
                                else BS.hGet input Cipher.nonceLength
                            unless (BS.length nonceBase == Cipher.nonceLength) $
                              ioError (userError "Truncated nonce")
                            key <- Cipher.hashPassword password salt
                            let encrypt n = do
                                  plaintext <- BS.hGet input Cipher.bufferLength
                                  traverse_ (\b -> incProgress b $ BS.length plaintext) pb
                                  let isLast = BS.length plaintext < Cipher.bufferLength
                                  nonce <- either (ioError . userError) pure (Cipher.makeNonce nonceBase n isLast)
                                  (ciphertext, tag) <- either (ioError . userError) pure (Cipher.encrypt plaintext key nonce)
                                  BS.hPut output ciphertext
                                  BS.hPut output tag
                                  unless isLast (encrypt (n + 1))
                                decrypt n = do
                                  chunk <- BS.hGet input (Cipher.bufferLength + Cipher.tagLength)
                                  traverse_ (\b -> incProgress b $ BS.length chunk) pb
                                  when (BS.length chunk < Cipher.tagLength) $
                                    ioError (userError "Truncated encrypted stream (missing final chunk or authentication tag)")
                                  let ciphertextLength = BS.length chunk - Cipher.tagLength
                                      (ciphertext, tag) = BS.splitAt ciphertextLength chunk
                                      isLast = ciphertextLength < Cipher.bufferLength
                                  nonce <- either (ioError . userError) pure (Cipher.makeNonce nonceBase n isLast)
                                  plaintext <- either (ioError . userError) pure (Cipher.decrypt ciphertext key nonce tag)
                                  when isLast $ do
                                    trailing <- BS.hGet input 1
                                    unless (BS.null trailing) $
                                      ioError (userError "Unexpected data after final chunk")
                                  BS.hPut output plaintext
                                  unless isLast (decrypt (n + 1))
                            if Cli.encrypt cli then encrypt 0 else decrypt 0
                        )
                  )
    )
    ( \err -> do
        hPutStrLn stderr ("htek: " ++ ioeGetErrorString err)
        exitFailure
    )
