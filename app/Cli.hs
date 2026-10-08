module Cli (Cli (..), parse, app_version, cli_help) where

app_version :: String
app_version = "htek 0.1.0.0"

cli_help :: String
cli_help =
  unlines
    [ app_version,
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
      | otherwise -> Left $ "unknown arg/flag " ++ h

expandFlags :: [String] -> [String]
expandFlags [] = []
expandFlags (('-' : '-' : flag) : rest) =
  ("--" ++ flag) : expandFlags rest
expandFlags (('-' : flags) : rest) =
  map (\c -> ['-', c]) flags ++ expandFlags rest
expandFlags (arg : rest) =
  arg : expandFlags rest

parse :: [String] -> Either String Cli
parse args = case parse' (expandFlags args) Nothing $ Cli Nothing Nothing False Nothing Nothing False False False False of
  Left err -> Left err
  Right cli ->
    if help cli
      then Right cli
      else
        if (not $ encrypt cli) && (not $ decrypt cli)
          then
            Left "neither encrypt or decrypt set. See --help for details."
          else
            if encrypt cli && decrypt cli
              then
                Left "both encrypt and decrypt set. See --help for details."
              else
                Right cli
