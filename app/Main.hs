import Parser
import Rules
import System.Environment
import Control.Monad
import Data.List

data Rule = Rule { name :: String
                 , _description :: String
                 , getRule :: Check
                 }

instance Eq Rule where
  r1 == r2 = name r1 == name r2

data Conf = Conf { showFct :: Warn -> String
                 , rules :: [Rule]
                 }

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

defaultConf :: Conf
defaultConf = Conf showLong allRules 

rulesLookup :: String -> [Rule] -> Maybe Rule
rulesLookup s = lookup s . map (\ r -> (name r, r))

showLong :: Warn -> String
showLong = show

showShort :: Warn -> String
showShort (Warn w (f, l)) = f ++ ":" ++ show l ++ ":" ++ fst (issues w)

doOne :: Conf -> String -> IO ()
doOne (Conf sFct rls) filename = do
  buff <- parseFile filename
  case buff of
    Right lst -> let rs = map getRule rls
                     warnings = sort $ join $ map (\ f -> f lst) rs
                 in mapM_ (putStrLn . sFct) warnings
    Left err -> putStrLn $ "unable to load file: "++ show (err :: IOError)

usage :: IO ()
usage = putStrLn (unwords ["usage: hsc [--short] [--long]",
                           "[--disable rule] [--enable rule] [files]"])
        >> putStrLn "  - Rules:"
        >> mapM_ displayRule allRules
  where displayRule (Rule n desc _) =
          putStrLn ("    * "++ n ++":\n      "++ desc)

doArgs :: Conf -> [String] -> Maybe (Conf, [String])
doArgs _ [] = Nothing
doArgs _ ["-h"] = Nothing
doArgs _ ["--help"] = Nothing
doArgs conf ("--short":xs) = doArgs (conf{ showFct=showShort }) xs
doArgs conf ("--long":xs) = doArgs (conf{ showFct=showLong }) xs
doArgs conf ("--disable":y:xs)
  | y `elem` map name (rules conf) = doArgs (conf{ rules = newRules}) xs
  | otherwise = Nothing
  where newRules = [r | r <- rules conf, name r /= y ]
doArgs conf@(Conf _ rls) ("--enable":y:xs) =
  rulesLookup y allRules >>= updateRules
  where updateRules r = doArgs (conf {rules=newRules r}) xs
        newRules r = if rls == allRules then [r] else rls++[r]
doArgs conf lst = Just (conf, lst)
        
main :: IO ()
main = getArgs >>= processAll . doArgs defaultConf
  where processAll Nothing = usage
        processAll (Just (conf,files)) = mapM_ (doOne conf) files
          
