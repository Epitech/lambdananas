module Conf (
doArgs,
Conf (Conf),
Rule (Rule),
allRules,
getRule,
optParser,
) where

import Options.Applicative
import Rules

data Rule = Rule { name :: String
                 , _description :: String
                 , getRule :: Check
                 }

instance Eq Rule where
  r1 == r2 = name r1 == name r2

-- | Holds the command line argument parsing result.
-- The 'Conf'' data aims at replacing the 'Conf' data.
data Conf' = Conf' { usage :: Bool
                   , descriptions :: Bool
                   , veraCompatible :: Bool
                   , silent :: Bool
                   , directories :: [FilePath]
                   }
                   deriving (Show)

parseDirectoryList :: String -> Maybe [FilePath]
parseDirectoryList s = Just [s]

optParser :: Parser Conf'
optParser = let
            descriptionHelp = "Outputs a description for each infraction"
            veraHelp = "Enables vera compatibility"
            in Conf'
            <$> switch
                (long "help"
                 <> short 'h'
                 <> help "Shows this message")
            <*> switch
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
            <*> option (maybeReader parseDirectoryList)
                (long "directories"
                 <> short 'd'
                 <> help "Directories to search")

data Conf = Conf { showFct :: Warn -> String
                 , rules :: [Rule]
                 , dirs :: [String]
                 }

allRules :: [Rule]
allRules = [ ruleCheckSign, ruleCheckIfs, ruleCheckReturns,
            ruleCheckDos, ruleCheckGuards, ruleCheckLines ] --, ruleCheckFuncs ]

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

defaultConf :: Conf
defaultConf = Conf showLong defaultRules []

rulesLookup :: String -> [Rule] -> Maybe Rule
rulesLookup s = lookup s . map (\ r -> (name r, r))

showLong :: Warn -> String
showLong = show

showShort :: Warn -> String
showShort (Warn w (f, l)) = f ++ ":" ++ show l ++ ":" ++ fst (issues w)

-- TODO : replace those with optparse applicative




doOpt :: Conf -> [String] -> Either String (Conf, [String])
doOpt _ ("-h":_) = Left "usage"
doOpt _ ("--help":_) = Left "usage"
doOpt conf ("--short":xs) = Right (conf{ showFct=showShort }, xs)
doOpt conf ("--long":xs) = Right (conf{ showFct=showLong }, xs)
doOpt conf ("--disable":y:xs)
  | y `elem` map name (rules conf) = Right (conf{ rules=newRules}, xs)
  | otherwise = Left ("unknown rule: '"++y++"'")
  where newRules = [r | r <- rules conf, name r /= y ]
doOpt conf@(Conf _ rls _) ("--enable":y:xs) = case rulesLookup y allRules of
  Just r -> Right $ updateRules r
  Nothing -> Left ("unknown rule: '" ++ y ++ "'")
  where updateRules r = (conf {rules=newRules r}, xs)
        newRules r = if rls == defaultRules then [r] else r:rls
doOpt conf ("-d":y:xs) = Right (conf {dirs=y:dirs conf}, xs)

doOpt _ (x:_) = Left ("unkown option: "++x)
doOpt _ _ = Left "shouldn't be there :("

doArgs :: Conf -> [String] -> Either String (Conf, [String])
doArgs (Conf _ _ []) [] = Left "Error: no file given"
doArgs conf args@(('-':_):_) = doOpt conf args >>= uncurry doArgs
doArgs conf lst = Right (conf, lst)
