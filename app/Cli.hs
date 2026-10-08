module Cli (Cli (..), parse, appVersion, cliHelp) where

appVersion :: String
appVersion = "htek 0.1.0.0"

cliHelp :: String
cliHelp =
  unlines
    [ appVersion,
      "Secure XChaCha20Poly1305 encryption using Argon2id for key derivation.",
      "",
      "Usage: htek [OPTIONS]",
      "",
      "Options:",
      "  -i, --input PATH       Input file path",
      "  -o, --output PATH      Output file path",
      "  -f, --force            Overwrite the output file if it exists",
      "  -k, --key-file PATH    Key file path",
      "  -p, --password PASS    Password for encryption or decryption",
      "  -e, --encrypt          Encrypt the input file",
      "  -d, --decrypt          Decrypt the input file",
      "  -v, --version          Show version information",
      "  -h, --help             Show this help message",
      "",
      "Examples:",
      "  htek -e -i plain.txt -o encrypted.bin",
      "  htek -d -i encrypted.bin -o plain.txt",
      "  htek -e -i plain.txt -o encrypted.bin -p mypassword",
      "  htek -d -i encrypted.bin -o plain.txt -p mypassword",
      "  htek -e -i plain.txt -o encrypted.bin -k secret.key",
      "  htek -d -i encrypted.bin -o plain.txt -k secret.key",
      ""
    ]

data Cli = Cli
  { input :: Maybe String,
    output :: Maybe String,
    force :: Bool,
    key_file :: Maybe String,
    password :: Maybe String,
    encrypt :: Bool,
    decrypt :: Bool,
    version :: Bool,
    help :: Bool
  }
  deriving (Show, Eq)

data Pending = Input | Output | KeyFile | Password deriving (Show, Eq)

parse' :: [String] -> Maybe Pending -> Cli -> Either String Cli
parse' args pending cli = case pending of
  Just Input -> case args of
    [] -> Left "missing input path"
    h : t -> parse' t Nothing $ cli {input = Just h}
  Just Output -> case args of
    [] -> Left "missing output path"
    h : t -> parse' t Nothing $ cli {output = Just h}
  Just KeyFile -> case args of
    [] -> Left "missing key file path"
    h : t -> parse' t Nothing $ cli {key_file = Just h}
  Just Password -> case args of
    [] -> Left "missing password"
    h : t -> parse' t Nothing $ cli {password = Just h}
  Nothing -> case args of
    [] -> Right cli
    h : t
      | h `elem` ["--input", "-i"] -> parse' t (Just Input) cli
      | h `elem` ["--output", "-o"] -> parse' t (Just Output) cli
      | h `elem` ["--force", "-f"] -> parse' t Nothing $ cli {force = True}
      | h `elem` ["--key-file", "-k"] -> parse' t (Just KeyFile) cli
      | h `elem` ["--password", "-p"] -> parse' t (Just Password) cli
      | h `elem` ["--encrypt", "-e"] -> parse' t Nothing $ cli {encrypt = True}
      | h `elem` ["--decrypt", "-d"] -> parse' t Nothing $ cli {decrypt = True}
      | h `elem` ["--version", "-v"] -> parse' t Nothing $ cli {version = True}
      | h `elem` ["--help", "-h"] -> parse' t Nothing $ cli {help = True}
      | otherwise -> case h of
          '-' : first : second : rest
            | first /= '-' ->
                parse' (map (\c -> ['-', c]) (first : second : rest) ++ t) Nothing cli
          _ -> Left $ "unknown arg/flag " ++ h

parse :: [String] -> Either String Cli
parse args = case parse' args Nothing $ Cli Nothing Nothing False Nothing Nothing False False False False of
  Left err -> Left err
  Right cli
    | help cli || version cli -> Right cli
    | not (encrypt cli || decrypt cli) ->
        Left "neither encrypt nor decrypt set. See --help for details."
    | encrypt cli && decrypt cli ->
        Left "both encrypt and decrypt set. See --help for details."
    | otherwise -> Right cli
