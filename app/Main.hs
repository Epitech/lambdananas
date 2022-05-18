{-|
Main module for the haskell style checker program.
-}
module Main where

import Conf
import Input
import Output

import Control.Monad
import Data.List
import System.IO
import Options.Applicative

main :: IO ()
main = execParser options >>= process
  where
    options = info (optParser <**> helper)
            (fullDesc
            <> header "Haskell Style Checker - An EPITECH Haskell Linter")

-- | Top level compute function.
-- Is called after the cli arguments have been parsed
process :: Conf -> IO ()
process (Conf _ (Just Dump) _ ) =
  putStrLn dumpManifest
process conf@(Conf _ _ []) =
  getContents >>= processMultiple conf . lines
process conf@(Conf _ _ paths) =
  loadAll paths >>= processMultiple conf

-- | Returns a complete list of paths needing to be checked.
loadAll :: [FilePath] -- ^ Directories to be loaded
        -> IO [FilePath] -- ^ A list of files to be checked
loadAll d = join <$> mapM load d

-- | Checks the coding style for a list of files.
processMultiple :: Conf -> [FilePath] -> IO()
processMultiple conf haskellFiles = case haskellFiles of
  [] -> hPutStrLn stderr $ errorMsg "no files or directories"
  nonEmptyFiles -> mapM_ (processOne conf) nonEmptyFiles

-- | Checks the coding style for a single file.
-- We are not returning a list of issues for performance reasons!
processOne :: Conf -> FilePath -> IO ()
processOne conf filename = do
  buff <- parseFile filename
  case buff of
    Right lst -> let rs = map getRule rls
                     warnings = sort $ join $ map (\ f -> f lst) rs
                 in mapM_ (outputOne conf) warnings
    Left err -> outputOneErr conf err
  where
    rls = defaultRules -- [Rule]

-- | Creates an error message appending it to `Error :`.
errorMsg :: String -> String
errorMsg = (++) "Error: "

