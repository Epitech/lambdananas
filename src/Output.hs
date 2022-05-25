{-|
Higher level module for style checker computations.
-}
module Output (
  outputOne,
  outputOneErr,
  outputVague,
  outputManifest,
  module Parser,
  module Rules,
  module Warn,
) where

import Conf
import Rules
import Parser
import Warn

import Data.Maybe
import Data.List

-- | Lookup table of gravities linked to their path.
-- Used in argos mode only.
argoOutputFiles :: [(Gravity, FilePath)]
argoOutputFiles = zip gravities fileNames
  where
    fileNames = createArgoFileName <$> ["major", "minor", "info"]
    gravities = [Major, Minor, Info]

-- | Creates a file name suitable for argos output mode.
createArgoFileName :: String -> String
createArgoFileName s = "style-" <> s <> ".txt"

-- | Output the result of a single coding style issue.
outputOne :: Conf -> Warn -> IO ()
outputOne Conf {mode = Just Silent} _ = return ()
outputOne Conf {mode = Just Argos} w@Warn {issue = i} =
    appendFile atPath $ showArgos w <> "\n"
  where
    atPath = fromMaybe errorsPath $ lookup (getGravity i) argoOutputFiles
    getGravity a = case lookupIssueInfo a of IssueInfo {gravity=g} -> g
    errorsPath = createArgoFileName "debug"
outputOne _ w = putStrLn $ showVera w

-- TODO : this is very much temporary and should be patched when
-- switching backend parsing library.
-- | Outputs a single error when the file could not be parsed.
outputOneErr :: Conf -> ParseError -> IO ()
outputOneErr Conf {mode = Just Silent} _ =
  return ()
outputOneErr Conf {mode = Just Argos} (ParseError filename loc _ text)
    | "Parse error:" `isPrefixOf` text = appendFile atPath $
      showArgos notParsableIssue <> "\n"
    -- everything not a parse error is an extension error
    | otherwise = appendFile "banned_funcs" $
      showArgos forbiddenExtIssue <> "\n"
  where
    atPath = fromMaybe errorsPath $ lookup Major argoOutputFiles
    errorsPath = createArgoFileName "debug"
    notParsableIssue = makeWarn NotParsable (filename, loc) $ StringArg filename
    forbiddenExtIssue = makeWarn ForbiddenExt (filename, loc) $ StringArg filename
outputOneErr _ (ParseError filename loc _ text)
    | "Parse error:" `isPrefixOf` text = putStrLn $ showVera issue
    -- everything not a parse error is an extension error
    | otherwise = putStrLn $ showDetails (lookupIssueInfo ForbiddenExt) (StringArg filename)
  where
    issue = makeWarn NotParsable (filename, loc) $ StringArg filename

-- | Dumps a manifest of all coding style issues in format
-- `<code>:<description>`.
outputManifest :: String
outputManifest = foldr mergeLines "" (merge <$> issues)
  where
    merge (_, IssueInfo {code = c, showDetails = d}) = c ++ ':':d NoArg
    mergeLines e acc = e ++ '\n':acc

-- | Appends a vague description of issues to 'style-student.txt'.
outputVague :: Conf -> [Issue] -> IO ()
outputVague _ [] = return ()
outputVague Conf{mode=Just Argos} i = appendFile (createArgoFileName "student") (toString $ accumulate i)
  where
    accumulate :: [Issue] -> [(Issue, Int)]
    accumulate = foldr accumulateVague []
    toString :: [(Issue, Int)] -> String
    toString l = concat $ ( \ (i', x) -> showVague i' x) <$> l
outputVague _ _ = return ()

accumulateVague :: Issue -> [(Issue, Int)] -> [(Issue, Int)]
accumulateVague i acc = case lookup i acc of
                          Nothing -> (i, 1):acc
                          Just n -> (i, n + 1):delete (i, n) acc

showArgos :: Warn -> String
showArgos w@Warn {issue = i} =
    filename ++ ':':show issueLine ++ ':':issueCode
  where
    info = lookupIssueInfo i
    issueCode = code info
    issueLine = snd $ loc w
    filename = fst $ loc w

showVera :: Warn -> String
showVera w@Warn {issue = i, arg = a} =
    filename ++ ':':issueLine ++ ':':issueGravity ++ ':':' ':issueCode ++
    " # " ++ issueDesc
  where
    info = lookupIssueInfo i
    issueDesc = showDetails info a
    issueCode = code info
    issueLine = show $ snd $ loc w
    issueGravity = show $ gravity info
    filename = fst $ loc w

-- | Creates a vague description of a given issue.
showVague :: Issue    -- ^ issue
          -> Int      -- ^ number of times it was raised
          -> String
showVague i n =
  issueCode ++ " rule has been violated " ++ show n ++ " times: " ++ details ++ "\n"
  where
    issueCode = code $ lookupIssueInfo i
    details = (showDetails $ lookupIssueInfo i) NoArg
