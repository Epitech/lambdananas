{-|
CLI argument configuration.
-}
module Conf (
  Conf (..),
  OutputModes (..),
  optParser,
) where

import Options.Applicative

-- | Enumerate the different coding style error output modes.
data OutputModes = Silent -- ^ Do not output anything (usefull for tests maybe ?)
                 | Argos -- ^ Output to style-(minor|major|info).txt (compatible with argos output)
                 | Vera -- ^ Output everything to stdout (compatible with vera++ output)
                 deriving Show

instance Read OutputModes where
  readsPrec _ "silent" = [(Silent, "")]
  readsPrec _ "argos" = [(Argos, "")]
  readsPrec _ "vera" = [(Vera, "")]
  readsPrec _ _ = []

-- | Holds the command line argument parsing result.
-- The 'Conf' data aims at replacing the 'Conf' data.
data Conf = Conf { mode :: Maybe OutputModes
                 , files :: [FilePath]
                 }
                 deriving Show

-- | Create a 'Conf when the returned parser is ran.
optParser :: Parser Conf
optParser = Conf
            <$> optional (option auto
                (long "output" <> short 'o' <> help outputHelp))
            <*> many (strArgument
                (metavar "FILE" <> help "Files to search"))
          where
            outputHelp = "Outputs the coding style issues in a specific way, \
            \can be 'silent', 'argos' or 'vera'."

