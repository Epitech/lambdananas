{-|
Higher level module for style checker computations.
-}
module Compute (
  outputOne,
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
outputOne (Conf True _ _ _ _) w = putStrLn $ showArgo w
outputOne (Conf _ True _ _ _) w = appendFile "style-major.txt" $ showVera w
outputOne (Conf _ _ True _ _) _ = return ()
outputOne _ w = print w
