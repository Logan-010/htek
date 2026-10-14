module Cipher (password, nonceLength, saltLength, tagLength, bufferLength, hashPassword, makeNonce, encrypt, decrypt) where

import Control.Exception (bracket)
import qualified Crypto.Cipher.ChaChaPoly1305 as C
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import qualified Crypto.KDF.Argon2 as Argon2
import Data.Bits (shiftR, (.&.), (.|.))
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word8)
import System.IO (hFlush, hGetEcho, hPutStr, hPutStrLn, hSetEcho, stderr, stdin)

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
prompt p = do
  hPutStr stderr p
  hFlush stderr
  passwordText <- bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
    hSetEcho stdin False
    getLine
  hPutStrLn stderr ""
  pure passwordText

password :: Bool -> IO String
password reprompt = do
  first <- prompt "Enter password: "
  if reprompt
    then do
      second <- prompt "(Re)enter password: "
      if first == second
        then pure first
        else ioError (userError "Passwords do not match")
    else pure first

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

makeNonce :: ByteString -> Int -> Bool -> Either String ByteString
makeNonce baseNonce counter isLast
  | counter < 0 || toInteger counter > 0x7FFFFFFF =
      Left "LE31 counter must be between 0 and 2^31 - 1"
  | otherwise =
      Right $ BS.append baseNonce (BS.pack bytes)
  where
    position :: Word32
    position = fromIntegral counter

    positionWithFlag :: Word32
    positionWithFlag =
      position .|. (if isLast then 0x80000000 else 0)

    bytes :: [Word8]
    bytes =
      [ fromIntegral ((positionWithFlag `shiftR` shift) .&. 0xFF)
      | shift <- [0, 8, 16, 24]
      ]

initCipher :: ByteString -> ByteString -> Either String C.State
initCipher key nonce =
  case (C.key key, C.nonce24 nonce) of
    (CryptoFailed e, _) -> Left (show e)
    (_, CryptoFailed e) -> Left (show e)
    (CryptoPassed k, CryptoPassed n) ->
      Right $ C.finalizeAAD (C.initializeX k n)

encrypt :: ByteString -> ByteString -> ByteString -> Either String (ByteString, ByteString)
encrypt plaintext key nonce
  | BS.length plaintext > bufferLength = Left "Invalid buffer length"
  | otherwise = do
      st <- initCipher key nonce
      let (ciphertext, st') = C.encrypt plaintext st
          tag = BA.convert (C.finalize st')
      pure (ciphertext, tag)

decrypt :: ByteString -> ByteString -> ByteString -> ByteString -> Either String ByteString
decrypt ciphertext key nonce expectedTag
  | BS.length ciphertext > bufferLength = Left "Invalid buffer length"
  | BS.length expectedTag /= tagLength = Left "Invalid authentication tag length"
  | otherwise = do
      st <- initCipher key nonce
      let (plaintext, st') = C.decrypt ciphertext st
          actualTag = BA.convert (C.finalize st') :: ByteString
      if BA.constEq expectedTag actualTag
        then pure plaintext
        else Left "Authentication failed"
