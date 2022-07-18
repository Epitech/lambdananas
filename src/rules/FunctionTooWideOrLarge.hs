{-|
Line too long or function too long.
-}

{-# LANGUAGE ExplicitForAll #-}

module FunctionTooWideOrLarge (
) where

import Data.Data
import Common
import Debug.Trace

{-
check :: ParseSuccess -> [Warn]
check m = trace (fun m) $ []

fun :: ParseSuccess -> String
fun m = fun' $ pt m

fun' :: forall d. Data d => d -> String
fun' d = show $ gmapQ check' d
  where
    check' :: forall d. Data d => d -> String
    check' e = case dataTypeOf of -}
