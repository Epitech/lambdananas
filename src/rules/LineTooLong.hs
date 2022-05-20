{-|
Line too long.
-}
module LineTooLong (
  description,
  hint,
  code,
  check,
) where

import Common

-- TODO : split this code in half ?

description :: Maybe [String] -> String
description _ = "line too long"

hint :: String
hint = "functions should be less than 10 lines and 80 columns"

code :: String
code = "F3"

gravity :: Gravity
gravity = Minor

check :: Check
check lst = uniqWarn $ join $ explore checkLine lst
  where checkLine (NDec (FunBind _ matches)) = foldMap checkLine' matches
        checkLine (NDec decl@PatBind{}) = checkLine' decl
        checkLine _ = []
        checkLine' decl = uniqFunWarn $ foldMap toWarn decl
        toWarn ssi@(SrcSpanInfo (SrcSpan _f l1 _c1 l2 c2) _) =
          [Warn FunctionTooBig (getLoc ssi) gravity | l2-l1 >= 10]
          ++
          [Warn LineTooLong (getLoc ssi) gravity | l1==l2 && c2 > 80]

uniqFunWarn :: [Warn] -> [Warn]
uniqFunWarn [] = []
uniqFunWarn (w1@(Warn FunctionTooBig _ Info):xs)
  | FunctionTooBig `elem` map what xs = uniqFunWarn xs
  | otherwise = w1 : uniqFunWarn xs
uniqFunWarn (x:xs) = x:uniqFunWarn xs

uniqWarn :: [Warn] -> [Warn]
uniqWarn [] = []
uniqWarn (x:xs) | x `elem` xs  = uniqWarn xs
                | otherwise = x : uniqWarn xs
