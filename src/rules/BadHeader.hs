{-|
Bad or no Epitech header.
-}
module BadHeader (
  check,
) where

import Common

check :: Check
check (_, comments, f) = case filter isHeader comments of
    [_] -> []
    _ -> [makeWarn BadHeader (f, 1) (StringArg f)]
  where
    isHeader (Comment True (SrcSpan _ 1 1 _ _) _) = True
    isHeader _ = False
