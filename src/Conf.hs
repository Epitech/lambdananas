{-|
Configuration.
-}
module Conf (
  Conf (Conf),
  optParser,
) where

import Options.Applicative

-- | Holds the command line argument parsing result.
-- The 'Conf' data aims at replacing the 'Conf' data.
data Conf = Conf { descriptions :: Bool
                   , veraCompatible :: Bool
                   , silent :: Bool
                   , directories :: Maybe [FilePath]
                   , files :: Maybe [FilePath]
                   }
                   deriving (Show)

-- | Determine a list of path from a string.
parsePathList :: String -> Maybe [FilePath]
parsePathList s = Just $ words s

-- | Create a 'Conf when the returned parser is ran.
optParser :: Parser Conf
optParser = let
            descriptionHelp = "Outputs a description for each infraction"
            veraHelp = "Enables vera compatibility"
            in Conf
            <$> switch
                (long "descriptions"
                 <> short 'v'
                 <> help descriptionHelp)
            <*> switch
                (long "vera-compatible"
                 <> short 'c'
                 <> help veraHelp)
            <*> switch
                (long "silent"
                 <> short 's'
                 <> help "Disables output to stdout and stderr")
            <*> optional (option (maybeReader parsePathList)
                (long "directories"
                 <> short 'd'
                 <> metavar "DIR"
                 <> help "Directories to search"))
            <*> optional (option (maybeReader parsePathList)
                (long "files"
                 <> short 'f'
                 <> metavar "FILE"
                 <> help "Files to search"))

