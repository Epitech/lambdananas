{-# LANGUAGE DeriveFoldable #-}

module Main where

import System.Environment
import Text.Read
import Data.Maybe

data BTree a = Leaf a | Trunk (BTree a) (BTree a) deriving Foldable

createSimpleBTree :: BTree Int
createSimpleBTree = Trunk (Leaf 1) (Trunk (Leaf 2) (Leaf 4))

main :: IO ()
main = print (foldr (\e acc -> show e <> acc) "" createSimpleBTree)
