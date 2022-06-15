{-|
Common imports for all rules to use.
-}
module Common (
  unL,
  mF,
  internalFail,
  module Warn,
  module GHC.Hs,
  module Control.Monad,
  module SrcLoc,
) where

import Warn

import Control.Monad
import Data.Maybe
import SrcLoc
import GHC.Hs hiding (NoSig)
import FastString

-- | Message to show on internal failures.
internalFail :: String
internalFail = "internal failure occured"

-- | Removes location informations.
unL :: [Located a] -> [a]
unL l = extract <$> l
  where
    extract (L _ a) = a

-- | Get a file name from a 'HsModule'.
mF :: HsModule GhcPs -> String
mF m = extract $ hsmodDecls m
  where
    extract (L s _:_) = unpackFS $
      fromMaybe (mkFastString "unknown file") $ srcSpanFileName_maybe s
    extract _ = "filename failure"
