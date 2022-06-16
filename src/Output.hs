{-|
Higher level module for style checker computations.
-}
module Output (
  outputOne,
  outputOneErr,
  outputVague,
  outputManifest,
  mkArgosFileName,
  module ParserWrapper,
  module Rules,
  module Warn,
) where

import Conf
import Rules
import ParserWrapper hiding (filename)
import Warn

import Data.Maybe
import Data.List

-- | Lookup table of gravities linked to their path.
-- Used in argos mode only.
argosGravityFiles :: [(Gravity, FilePath)]
argosGravityFiles = zip gravities filenames
  where
    filenames = mkArgosFileName <$> ["major", "minor", "info"]
    gravities = [Major, Minor, Info]

-- | Creates a file name suitable for argos output mode.
mkArgosFileName :: String -> String
mkArgosFileName s = "style-" <> s <> ".txt"

-- | Output the result of a single coding style issue.
outputOne :: Conf -> Warn -> IO ()
outputOne Conf {mode = Just Silent} _ = return ()
outputOne Conf {mode = Just Argos} w@Warn {issue = i} =
    appendFile atPath $ showArgos w <> "\n"
  where
    atPath = fromMaybe errorsPath $ lookup (getGravity i) argosGravityFiles
    getGravity a = case lookupIssueInfo a of IssueInfo {gravity=g} -> g
    errorsPath = mkArgosFileName "debug"
outputOne _ w = putStrLn $ showVera w

-- TODO : this is very much temporary and should be patched when
-- switching backend parsing library.
-- | Outputs a single error when the file could not be parsed.
outputOneErr :: Conf -> ParseError -> IO ()
outputOneErr _ _ =
  return ()

-- | Dumps a manifest of all coding style issues in format
-- `<code>:<description>`.
outputManifest :: String
outputManifest = intercalate "\n" (createLine <$> issues)
  where
    createLine (_, IssueInfo {code = c, showDetails = d}) = c ++ ':':d NoArg

-- | Appends a vague description of issues to 'style-student.txt'.
outputVague :: [Issue] -> String
outputVague i = (++ "\n") . intercalate "\n" $ uncurry showVague <$>
    removeNoOccurences (count <$> occurenceList)
  where
    removeNoOccurences l = filter (\(_, y) -> y /= 0) l
    count (x, _, _) = (x, length $ filter (== x) i)
    occurenceList = [(x, y, 0 :: Int)| (x, y) <- issues]

-- | Creates a vague description of a given issue.
showVague :: Issue    -- ^ issue
          -> Int      -- ^ number of times it was raised
          -> String
showVague i n =
  issueCode ++ " rule has been violated " ++ show n ++ " times: " ++ details
  where
    issueCode = code $ lookupIssueInfo i
    details = (showDetails $ lookupIssueInfo i) NoArg

-- | Produce a warning in argos format.
showArgos :: Warn -> String
showArgos w@Warn {issue = i} =
    filename ++ ':':show issueLine ++ ':':issueCode
  where
    info = lookupIssueInfo i
    issueCode = code info
    issueLine = snd $ loc w
    filename = fst $ loc w

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
