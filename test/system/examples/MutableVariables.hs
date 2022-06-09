{-
-- EPITECH PROJECT, 2022
-- foo
-- File description:
-- bar
-}
module Main where

import System.Environment
import Text.Read
import Data.Maybe
import Data.IORef

newtype Counter = Counter { x :: IORef Int }

makeCounter :: Int -> IO Counter
makeCounter i = do iref <- newIORef i
                   return (Counter iref)

incCounter :: Int -> Counter -> IO ()
incCounter i (Counter c) = modifyIORef c (+ i)

showCounter :: Counter -> IO ()
showCounter (Counter c) = readIORef c >>= print

main :: IO ()
main = do
    counter <- makeCounter 1
    incCounter 1 counter
    showCounter counter
