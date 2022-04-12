module Main where

import Parser
import Conf
import Files
import Control.Monad
import Data.List
import System.IO
import Options.Applicative

main :: IO ()
main = execParser options >>= process where
  options = info (optParser <**> helper)
            (fullDesc
            <> header "Haskell Style Checker - An EPITECH Haskell Linter")

-- | Top level compute function.
-- Is called after the cli arguments have been parsed.
process :: Conf' -> IO ()
-- Vera compatible compute
process (Conf' _ True _ Nothing Nothing) = getLine >>= putStrLn
-- Normal compute
process conf@(Conf' _ True _ (Just d) (Just f)) = do
  dir <- join <$> mapM loadDir d
  mapM_ (processOne $ conf) (f ++ dir)
process _ = hPutStrLn stderr $ errorMsg "failed to interpret cli options"

-- | Given a 'Conf'', checks the coding style for a single file.
processOne :: Conf' -> FilePath -> IO ()
processOne _ filename = do
  buff <- parseFile filename
  case buff of
    Right lst -> let rs = map getRule rls
                     warnings = sort $ join $ map (\ f -> f lst) rs
                 in mapM_ (putStrLn . sFct) warnings
    Left err -> putStrLn $ errorMsg $ "Unable to load file: " ++ show (err :: IOError) -- TODO : check for extensions here by watching for the error returned
  where
    sFct = showLong
    rls = defaultRules

-- | Creates an error message appending it to `Error :`.
errorMsg :: String -> String
errorMsg = (++) "Error: "

