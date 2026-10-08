module Crypto (password) where

import GHC.IO.Handle (hSetEcho)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdin)

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
