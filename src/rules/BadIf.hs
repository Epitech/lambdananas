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

description :: Maybe [String] -> String
description _ = "nested if"

hint :: String
hint = "ifs constructs should not be nested"

code :: String
code = "C1"

gravity :: Gravity
gravity = Major

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
