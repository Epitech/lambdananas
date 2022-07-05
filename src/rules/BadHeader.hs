{-|
Bad or no Epitech header.
-}
module BadHeader (
  check,
) where

import Common
import Text.Regex.TDFA
import Data.List

check :: ParseSuccess -> [Warn]
check m = genWarn $ find startComment $ comments m
  where
    startComment :: Located AnnotationComment -> Bool
    startComment (L (RealSrcSpan l) (AnnBlockComment s)) =
      srcSpanStartLine l == 1 && srcSpanStartCol l == 1 && s =~ regex
    startComment _ = False
    genWarn :: Maybe (Located AnnotationComment) -> [Warn]
    genWarn Nothing = [mkWarn BadHeader (file, 1) (StringArg file)]
    genWarn _ = []
    file :: String
    file = mF $ pt m

regex :: String
regex = "-- EPITECH PROJECT, [0-9]+\n--.*\n-- File description:\n-- .*$"
