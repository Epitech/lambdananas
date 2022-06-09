{-
-- EPITECH PROJECT, 2022
-- foo
-- File description:
-- bar
-}
module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if (args !! 1) == "foo"
    then print "yay"
  else if (args !! 1) == "bar"
    then print "yay"
  else
    print "nay"

