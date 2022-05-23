{-|
Bad ifs.
-}
module BadIf (
  description,
  hint,
  code,
  check,
) where

import Common

check :: Check
check = join . explore checkIf
  where checkIf (NExp (If ssi _ ift ife)) | countIfs ift ife >= 1 =
          [Warn BadIf (getLoc ssi) gravity]
        checkIf _ = []
        countIfs ifthen ifelse = inspectExpr countIf ifthen <>
                                 inspectExpr countIf ifelse
        countIf :: Node -> Sum Int
        countIf (NExp If{}) = Sum 1
        countIf _ = Sum 0
