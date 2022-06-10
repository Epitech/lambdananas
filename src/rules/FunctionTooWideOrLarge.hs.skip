{-|
Line too long or function too long.
-}
module FunctionTooWideOrLarge (
  check,
) where

import Common

check :: Check
check (lst, _, _) = uniqWarn $ join $ explore checkLine lst
  where checkLine (NDec (FunBind _ matches)) = foldMap checkLine' matches
        checkLine (NDec decl@PatBind{}) = checkLine' decl
        checkLine _ = []
        checkLine' decl = uniqFunWarn $ foldMap toWarn decl
        toWarn ssi@(SrcSpanInfo (SrcSpan _f l1 _c1 l2 c2) _) =
          [makeWarn FunctionTooBig (getLoc ssi) NoArg | l2-l1 >= 10]
          ++
          [makeWarn LineTooLong (getLoc ssi) NoArg | l1==l2 && c2 > 80]

uniqFunWarn :: [Warn] -> [Warn]
uniqFunWarn [] = []
uniqFunWarn (w1@(Warn FunctionTooBig _ _):xs)
  | FunctionTooBig `elem` map issue xs = uniqFunWarn xs
  | otherwise = w1 : uniqFunWarn xs
uniqFunWarn (x:xs) = x:uniqFunWarn xs

uniqWarn :: [Warn] -> [Warn]
uniqWarn [] = []
uniqWarn (x:xs) | x `elem` xs  = uniqWarn xs
                | otherwise = x : uniqWarn xs
