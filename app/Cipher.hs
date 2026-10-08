module Cipher (password, nonceLength, saltLength, bufferLength, hashPassword) where

import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import qualified Crypto.KDF.Argon2 as Argon2
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO (hFlush, hPutStr, hPutStrLn, hSetEcho, stderr, stdin)

nonceLength :: Int
nonceLength = 20

saltLength :: Int
saltLength = 32

bufferLength :: Int
bufferLength = 2 * 1024 * 1024

keyLength :: Int
keyLength = 32

tagLength :: Int
tagLength = 16

prompt :: String -> IO String
prompt p =
  hPutStr stderr p
    >> hFlush stderr
    >> hSetEcho stdin False
    >> getLine
    >>= \w ->
      hSetEcho stdin True
        >> hPutStrLn stderr ""
        >> pure w

password :: Bool -> IO String
password reprompt =
  if reprompt
    then
      prompt "Enter password: " >>= \p1 ->
        prompt "(Re)enter password: " >>= \p2 ->
          if p1 /= p2 then error "Passwords do not match" else pure p1
    else prompt "Enter password: "

argon2Options :: Argon2.Options
argon2Options =
  Argon2.Options
    { Argon2.iterations = 2,
      Argon2.memory = 19456,
      Argon2.parallelism = 1,
      Argon2.variant = Argon2.Argon2id,
      Argon2.version = Argon2.Version13
    }

hashPassword :: String -> ByteString -> IO ByteString
hashPassword pw salt = case Argon2.hash argon2Options (TE.encodeUtf8 (T.pack pw)) salt keyLength of
  CryptoPassed key -> pure key
  CryptoFailed err -> fail $ show err

