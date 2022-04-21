{-|
Main module for the haskell style checker program.
-}
module Main where

import Conf
import Files
import Compute

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
process conf@(Conf _ True _ Nothing Nothing) =
  getContents >>= processMultiple conf . lines
process conf@(Conf _ False _ directories files) =
  loadAll directories files >>= processMultiple conf
process _ = hPutStrLn stderr $ errorMsg "failed to interpret cli options"

-- | Returns a complete list of paths needing to be checked.
loadAll :: Maybe [FilePath] -- ^ Directories to be loaded
        -> Maybe [FilePath] -- ^ Files to be loaded
        -> IO [FilePath] -- ^ A list of files to be checked
loadAll (Just d) (Just f) = do
  loadedDirs <- join <$> mapM loadDir d
  return $ loadedDirs <> f
loadAll Nothing (Just f) = return f
loadAll (Just d) Nothing = join <$> mapM loadDir d
loadAll Nothing Nothing = return []

-- | Checks the coding style for a list of files.
processMultiple :: Conf -> [FilePath] -> IO()
processMultiple conf haskellFiles = do
  case haskellFiles of
    [] -> hPutStrLn stderr $ errorMsg "no files or directories"
    nonEmptyFiles -> mapM_ (processOne conf) nonEmptyFiles

-- | Checks the coding style for a single file.
-- We are not returning a list of issues for performance reasons!
processOne :: Conf -> FilePath -> IO ()
processOne conf filename = do
  buff <- parseFile filename -- buff is a Either IOError [Decl SrcSpanInfo]
  case buff of
    Right lst -> let rs = map getRule rls -- rs = [([Decl SrcSpanInfo] -> [Warn])]
                     warnings = sort $ join $ map (\ f -> f lst) rs -- [Warn] : sorted
                 in mapM_ (outputOne conf) warnings -- IO ()
    Left err -> putStrLn $ errorMsg $ "Unable to load file: " ++ show (err :: IOError) -- TODO : check for extensions here by watching for the error returned
  where
    rls = defaultRules -- [Rule]

-- | Creates an error message appending it to `Error :`.
errorMsg :: String -> String
errorMsg = (++) "Error: "

