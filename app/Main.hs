{-|
Main module for the haskell style checker program.
-}
module Main where

import ParserWrapper
import NoSig
import Warn

main = do
  res <- parseFile "app/Main.hs"
  case res of
    Right a -> mapM_ putStrLn $ showVera <$> check a
    Left _ -> error "fail"

-- | Produce a warning in vera format.
showVera :: Warn -> String
showVera w@Warn {issue = i, arg = a} =
    filename ++ ':':issueLine ++ ':':' ':issueGravity ++ ':':issueCode ++
    " # " ++ issueDesc
  where
    info = lookupIssueInfo i
    issueDesc = showDetails info a
    issueCode = code info
    issueLine = show $ snd $ loc w
    issueGravity = show $ gravity info
    filename = fst $ loc w

{-
import Conf
import Input
import Output

import Control.Monad
import Data.List
import Options.Applicative
import Data.Text (pack, split, unpack)

main :: IO ()
main = execParser options >>= process
  where
    options = info (optParser <**> helper)
            (fullDesc
            <> header "Haskell Style Checker - An EPITECH Haskell Linter")

-- | Top level compute function.
-- Is called after the cli arguments have been parsed
process :: Conf -> IO ()
process Conf{manifest = Just Dump} =
  putStrLn outputManifest
process conf@Conf{files = []} =
  getContents >>= loadAll conf . lines >>= processMultiple conf
process conf@Conf{files = paths} =
  loadAll conf paths >>= processMultiple conf

-- | Returns a complete list of paths needing to be checked.
loadAll :: Conf
        -> [FilePath] -- ^ Directories to be loaded
        -> IO [FilePath] -- ^ A list of files to be checked
loadAll Conf{excludeDirs = Just f} d =
  join <$> mapM (load $ unpack <$> split (==':') (pack f)) d
loadAll Conf{excludeDirs = Nothing} d =
  join <$> mapM (load []) d

-- | Checks the coding style for a list of files.
processMultiple :: Conf -> [FilePath] -> IO ()
processMultiple conf@Conf {mode = Just Argos} haskellFiles =
    mapM (processOne conf) haskellFiles >>=
    appendIfNotEmpty f . outputVague . concat
  where
    f = mkArgosFileName "student"
    appendIfNotEmpty _ "" = return ()
    appendIfNotEmpty f' s = appendFile f' s
processMultiple conf haskellFiles =
  mapM_ (processOne conf) haskellFiles

-- | Checks the coding style for a single file.
-- We are not returning a list of issues for performance reasons!
processOne :: Conf -> FilePath -> IO [Issue]
processOne conf filename = do
  buff <- parseFile filename
  case buff of
    Right lst -> let warnings = sort $ join $ map (\ f -> f lst) rls
      in mapM_ (outputOne conf) warnings >> return (extractIssue <$> warnings)
    Left err -> outputOneErr conf err >> return [NotParsable]
  where
    rls = defaultRules
    extractIssue Warn{issue = i} = i

-- | Creates an error message appending it to `Error :`.
errorMsg :: String -> String
errorMsg = (++) "Error: "
-}
