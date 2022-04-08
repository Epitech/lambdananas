module Conf (
Rule (Rule),
Conf (Conf, showFct, rules, dirs),
Conf' (Conf'),
allRules,
getRule,
optParser,
showLong,
defaultRules,
) where

import Options.Applicative
import Rules

data Rule = Rule { name :: String
                 , _description :: String
                 , getRule :: Check
                 }

instance Eq Rule where
  r1 == r2 = name r1 == name r2

data Conf = Conf {
      showFct :: Warn -> String,
      rules :: [Rule],
      dirs :: [String]
}

-- | Holds the command line argument parsing result.
-- The 'Conf'' data aims at replacing the 'Conf' data.
data Conf' = Conf' { descriptions :: Bool
                   , veraCompatible :: Bool
                   , silent :: Bool
                   , directories :: Maybe [FilePath]
                   , files :: Maybe [FilePath]
                   }
                   deriving (Show)

-- | Determine a list of path from a string.
parsePathList :: String -> Maybe [FilePath]
parsePathList s = Just $ words s

-- | Create a 'Conf'' when the returned parser is ran.
optParser :: Parser Conf'
optParser = let
            descriptionHelp = "Outputs a description for each infraction"
            veraHelp = "Enables vera compatibility"
            in Conf'
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

allRules :: [Rule]
allRules = [ ruleCheckSign, ruleCheckIfs, ruleCheckReturns,
            ruleCheckDos, ruleCheckGuards, ruleCheckLines ]

defaultRules :: [Rule]
defaultRules = [ ruleCheckSign, ruleCheckIfs, ruleCheckReturns,
                 ruleCheckDos, ruleCheckGuards, ruleCheckLines ]

ruleCheckSign :: Rule
ruleCheckSign = Rule "check-signatures"
                "top declaration has no corresponging type signature"
                checkSigs

ruleCheckIfs :: Rule
ruleCheckIfs = Rule "check-ifs"
               "nested if"
               checkIfs

ruleCheckReturns :: Rule
ruleCheckReturns = Rule "check-returns"
                   "useless return statement in do block"
                   checkReturns

ruleCheckDos :: Rule
ruleCheckDos = Rule "check-dos"
               "useless do"
               checkDos

ruleCheckGuards :: Rule
ruleCheckGuards = Rule "check-guards"
                  "guard should be pattern match"
                  checkGuards

ruleCheckLines :: Rule
ruleCheckLines = Rule "check-lines"
                 "functions should be less than 10 lines x 80 columns"
                 checkLines

rulesLookup :: String -> [Rule] -> Maybe Rule
rulesLookup s = lookup s . map (\ r -> (name r, r))

showLong :: Warn -> String
showLong = show

showShort :: Warn -> String
showShort (Warn w (f, l)) = f ++ ":" ++ show l ++ ":" ++ fst (issues w)
