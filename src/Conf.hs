{-|
CLI argument configuration.
-}
module Conf (
  Conf (..),
  OutputModes (..),
  ManifestDump (..),
  optParser,
) where

import Options.Applicative

-- | Enumerate the different coding style error output modes.
data OutputModes = Silent -- ^ Do not output anything (usefull for tests maybe ?)
                 | Argos -- ^ Output to style-(minor|major|info).txt (compatible with argos output)
                 | Vera -- ^ Output everything to stdout (compatible with vera++ output)
                 deriving Show

data ManifestDump = Dump -- ^ Dump a manifest of all error codes and their descriptions
              | NoDump -- ^ Do not dump (default)
              deriving Show

instance Read OutputModes where
  readsPrec _ "silent" = [(Silent, "")]
  readsPrec _ "argos" = [(Argos, "")]
  readsPrec _ "vera" = [(Vera, "")]
  readsPrec _ _ = []

-- | Holds the command line argument parsing result.
-- The 'Conf' data aims at replacing the 'Conf' data.
data Conf = Conf { mode :: Maybe OutputModes
                 , manifest :: Maybe ManifestDump -- ^ Should a manifest be dumped
                 , files :: [FilePath] -- ^ Files to be checked
                 }
                 deriving Show

-- | Create a 'Conf when the returned parser is ran.
optParser :: Parser Conf
optParser = Conf
            <$> optional (option auto
                (long "output" <> short 'o' <> help outputHelp))
            <*> optional (flag NoDump Dump
                (long "dump-manifest" <> help manifestHelp))
            <*> many (strArgument
                (metavar "FILE" <> help "Files to search"))
          where
            outputHelp = "Outputs the coding style issues in a specific way, \
            \can be 'silent', 'argos' or 'vera'"
            manifestHelp = "Dumps a manifest of all errors in the following format \
            \<code>:<description> while ignoring every other options"

