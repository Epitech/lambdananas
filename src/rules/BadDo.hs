{-|
Bad do.
-}

{-# LANGUAGE ExplicitForAll #-}

module BadDo (
  check,
) where

import Common
import Debug.Trace
import Data.Data

check :: ParseSuccess -> [Warn]
check m = fun =<< unL (hsmodDecls $ pt m)
  where
    fun :: HsDecl GhcPs -> [Warn]
    fun (ValD _ a) = trace (debug a) []
    fun _ = []

debug :: HsBind GhcPs -> String
debug a = join $ gmapQ f a
  where
    f :: Data d => d -> String
    f d = join $ constrFields $ toConstr d

