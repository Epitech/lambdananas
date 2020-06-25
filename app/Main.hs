import Parser
import Rules
import System.Environment
import Debug.Trace
import Data.Monoid
import Control.Monad
import Data.List

rules :: [Check]
rules = [ checkSigs, checkIfs, checkReturns, checkDos, checkGuards ]

doOne :: String -> IO ()
doOne filename = do
  --putStrLn $ "------ " ++ filename ++ ":"
  buff <- parseFile filename
  case buff of
    Right lst -> let warnings = sort $ join $ map (\ f -> f lst) rules
                 in mapM_ print warnings
    Left err -> putStrLn $ "unable to load file: "++ show (err :: IOError)
    
main :: IO ()
main = getArgs >>= mapM_ doOne

