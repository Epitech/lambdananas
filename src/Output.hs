{-|
Higher level module for style checker computations.
-}
module Output (
  outputOne,
  outputOneErr,
  argoOutputFiles,
  dumpManifest,
  module Parser,
  module Rules,
) where

import Conf
import Rules
import Parser

import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad (when)

-- | Table of gravities linked to their filepath dump.
-- Used in argos mode only.
argoOutputFiles :: [(Gravity, FilePath)]
argoOutputFiles = zip gravities fileNames
  where
    fileNames = createArgoFileName <$> ["major", "minor", "info"]
    gravities = [Major, Minor, Info]

-- | Given a 'String' creates a 'FilePath' of form :
-- `style-<string>.txt`
createArgoFileName :: String -> FilePath
createArgoFileName s = "style-" <> s <> ".txt"

-- | Output the result of a single coding style error.
outputOne :: Conf -> Warn -> IO ()
outputOne Conf {mode = Just Silent} _ = return ()
outputOne Conf {mode = Just Argos} w@(Warn _ _ g) =
    appendFile atPath $ showArgo w <> "\n"
  where
    atPath = fromMaybe errorsPath $ lookup g argoOutputFiles
    errorsPath = createArgoFileName "debug"
outputOne _ w = putStrLn $ showVera w

-- TODO : this is very much temporary and should be patched when
-- refactoring the way issues are handled.
-- | Outputs a single error when the file could not be parsed.
outputOneErr :: Conf -> String -> IO ()
outputOneErr Conf {mode = Just Silent} _ =
  return ()
outputOneErr Conf {mode = Just Argos} s
    | "Parse error:" `isPrefixOf` s = appendFile atPath $
      showArgo issue <> "\n"
    | otherwise = appendFile "banned_funcs" $ snd $ getIssueDesc $
      ForbiddenExt "file"
  where
    atPath = fromMaybe errorsPath $ lookup Major argoOutputFiles
    errorsPath = createArgoFileName "debug"
    issue = Warn (NotParsable "file") ("file", 0) Major
outputOneErr _ s
    | "Parse error:" `isPrefixOf` s = putStrLn $ showVera issue
    | otherwise = putStrLn $ snd $ getIssueDesc $ ForbiddenExt "file"
  where
    issue = Warn (NotParsable "file") ("file", 0) Major

-- | Dumps a manifest of all coding style issues in format
-- `<code>:<description>`.
dumpManifest :: String
dumpManifest = foldr mergeLines "" (merge <$> getIssuesList)
  where
    merge (a, b) = a ++ ':':b
    mergeLines e acc = e ++ '\n':acc
