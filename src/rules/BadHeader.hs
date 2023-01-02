{-|
Bad or no Epitech header.
-}
module BadHeader (
  check,
) where

import Common
import Text.Regex.TDFA

check :: Check
check (_, _, comments, f) = case filter isHeader comments of
    [_] -> []
    _ -> [makeWarn BadHeader (f, 1) (StringArg f)]
  where
    isHeader (Comment True (SrcSpan _ 1 1 _ _) s)
      | s =~ regex = True
      | otherwise = False
    isHeader _ = False
    regex = "-- EPITECH PROJECT, [0-9]+\n--.*\n-- File description:\n-- .*$"
