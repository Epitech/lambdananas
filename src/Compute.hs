{-|
Higher level module for style checker computations.
-}
module Compute (
  outputOne,
  argoOutputFiles,
  module Parser,
  module Rules,
) where

import Conf
import Rules
import Parser

-- | File list for argo output
argoOutputFiles :: [FilePath]
argoOutputFiles = (<> ".txt") <$> ("style-" <>) <$> ["major", "minor", "info"]

-- | Output the result of a single coding style error.
outputOne :: Conf -> Warn -> IO ()
outputOne (Conf (Just Silent) _) _ = return ()
outputOne (Conf (Just Argos) _) w = appendFile "style-major.txt" $ showArgo w
outputOne _ w = putStrLn $ showVera w
