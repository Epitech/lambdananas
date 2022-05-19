{-|
Bad ifs.
-}
module BadIf (
  description,
  code,
  check,
) where

import Common

description :: String
description = "nested if"

code :: String
code = "C1"

check :: Check
check = join . explore checkIf
  where checkIf (NExp (If ssi _ ift ife)) | countIfs ift ife >= 1 =
          [Warn BadIf (getLoc ssi) Major]
        checkIf _ = []
        countIfs ifthen ifelse = inspectExpr countIf ifthen <>
                                 inspectExpr countIf ifelse
        countIf :: Node -> Sum Int
        countIf (NExp If{}) = Sum 1
        countIf _ = Sum 0
