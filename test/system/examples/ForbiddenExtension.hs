{-
-- EPITECH PROJECT, 2022
-- foo
-- File description:
-- bar
-}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE GADTs #-}

module Main where

import System.Environment
import Text.Read
import Data.Maybe

data BTree a = Leaf a | Trunk (BTree a) (BTree a) deriving Foldable

fun :: Int -> String
fun = \ n -> case n of {1 -> "un"; 2 -> "deux"; 3 -> "trois"; _ -> "yoyele"}

data Term a where
    Lit    :: Int -> Term Int
    Succ   :: Term Int -> Term Int
    IsZero :: Term Int -> Term Bool
    If     :: Term Bool -> Term a -> Term a -> Term a
    Pair   :: Term a -> Term b -> Term (a,b)

eval :: Term a -> a
eval (Lit i)      = i
eval (Succ t)     = 1 + eval t
eval (IsZero t)   = eval t == 0
eval (If b e1 e2) = if eval b then eval e1 else eval e2
eval (Pair e1 e2) = (eval e1, eval e2)

createSimpleBTree :: BTree Int
createSimpleBTree = Trunk (Leaf 1) (Trunk (Leaf 2) (Leaf 4))

main :: IO ()
main = print (foldr (\e acc -> show e <> acc) "" createSimpleBTree)
