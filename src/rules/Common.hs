{-|
Common imports for all rules to use.
-}
module Common (
  unL,
  mF,
  module Warn,
  module GHC.Hs,
  module Control.Monad,
  module SrcLoc,
) where

import Control.Monad
import SrcLoc
import GHC.Hs hiding (NoSig)
import Warn
import FastString

-- | Removes location informations.
unL :: [Located a] -> [a]
unL l = extract <$> l
  where
    extract (L _ a) = a

-- | Get a file name from a 'HsModule'.
mF :: HsModule GhcPs -> String
mF m = extract $ hsmodDecls m
  where
    extract (L s _:_) = case srcSpanFileName_maybe s of
      Nothing -> "unknown file"
      Just s' -> unpackFS s'
    extract _ = "filename failure"
