module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  string <- return args
  print string
