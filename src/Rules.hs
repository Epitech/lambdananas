{-|
Coding style rules.
-}
module Rules (
    Check,
    Rule (..),
    checkSigs,
    checkIfs,
    checkReturns,
    checkDos,
    checkGuards,
    checkLines
) where

import Parser

import Language.Haskell.Exts.Syntax hiding (Rule)
import Language.Haskell.Exts.SrcLoc
import Control.Monad
import Control.Monad.Writer
import Data.Foldable

import BadIf
import NoSig
import BadDoReturn
import BadDo
import BadGuard
import FunctionTooWideOrLarge
import Common

checkIfs = BadIf.check
checkSigs = NoSig.check
checkReturns = BadDoReturn.check
checkDos = BadDo.check
checkGuards = BadGuard.check
checkLines = FunctionTooWideOrLarge.check

-- | Describes a coding style rule.
-- A rule can emit one or more 'Warn'.
data Rule = Rule { name :: String         -- ^ Rule name
                 , _description :: String -- ^ Rule description
                 , getRule :: Check       -- ^ Function to check the rule
                 }

instance Eq Rule where
  r1 == r2 = name r1 == name r2

