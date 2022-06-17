{-|
Common imports for all rules to use.
-}
module Common (
  unL,
  mF,
  internalFail,
  ParseSuccess (..),
  ParseError (..),
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
import ApiAnnotation

-- | Wrapper around a parse tree and comments produced by a successful parsing.
data ParseSuccess = ParseSuccess { pt :: HsModule GhcPs
                                 , comments :: [Located AnnotationComment]
                                 }

-- | Represents a failed parsing with location of the failure.
data ParseError = ParseError { filename :: String
                             , line :: Int
                             , column :: Int
                             } deriving Eq

instance Show ParseError where
  show (ParseError f l c) = "Failed to parse at : " ++ f ++ ' ':show l ++ ':':show c

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
