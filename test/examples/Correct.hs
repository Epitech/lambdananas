module Main where

import System.Environment
import Text.Read
import Data.Maybe

type Args = [String]

main :: IO ()
main = do
        args <- getArgs
        print $ (makeList . readArgs) args

readArgs :: Args -> Int
readArgs (x:_) = fromMaybe 0 $ readMaybe x
readArgs _ = 0

makeList :: Int -> [Int]
makeList i = [0..i]

