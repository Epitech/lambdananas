import Parser
import Rules
import System.Environment
import Debug.Trace
import Data.Monoid
import Control.Monad
import Data.List

data Rule = Rule { name :: String
                 , description :: String
                 , getRule :: Check
                 }

instance Eq Rule where
  r1 == r2 = name r1 == name r2

allRules :: [Rule]
allRules = [
  Rule "check-signatures"
  "top declaration has no corresponging type signature"
  checkSigs,
  Rule "check-ifs"
  "nested if"
  checkIfs,
  Rule "check-returns"
  "useless return statement in do block"
  checkReturns,
  Rule "check-dos"
  "useless do"
  checkDos,
  Rule "check-guards"
  "guard should be pattern match"
  checkGuards
  ]

rulesLook :: [Rule] -> [(String,Rule)]
rulesLook = map (\ r -> (name r, r))

doOne :: [Rule] -> String -> IO ()
doOne rules filename = do
  buff <- parseFile filename
  case buff of
    Right lst -> let rs = map getRule rules
                     warnings = sort $ join $ map (\ f -> f lst) rs
                 in mapM_ print warnings
    Left err -> putStrLn $ "unable to load file: "++ show (err :: IOError)

usage :: IO ()
usage = putStrLn "usage: hsc [--disable rule] [--enable rule] [files]"
        >> putStrLn "  - Rules:"
        >> mapM_ displayRule allRules
  where displayRule (Rule name description _) =
          putStrLn ("    * "++ name ++":\n      "++description)

doArgs :: [Rule] -> [String] -> Maybe ([Rule],[String])
doArgs rules [] = Nothing
doArgs rules ("--disable":y:xs)
  | y `elem` map name rules = doArgs [r | r <- rules, name r /= y ] xs
  | otherwise = Nothing
doArgs rules ("--enable":y:xs) = lookup y (rulesLook allRules) >>=
  \ r -> doArgs (if rules == allRules then [r] else rules++[r]) xs
doArgs rules lst = Just (rules, lst)
        
main :: IO ()
main = getArgs >>= processAll . doArgs allRules
  where processAll Nothing = usage
        processAll (Just (rules, files)) = mapM_ (doOne rules) files
