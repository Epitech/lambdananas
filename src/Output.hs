{-|
Higher level module for style checker computations.
-}
module Output (
  outputOne,
  argoOutputFiles,
  module Parser,
  module Rules,
) where

import Conf
import Rules
import Parser

import Data.Maybe

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
