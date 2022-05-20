{-|
Coding style rules.
-}
module Rules (
    Check,
    Warn (..),
    getIssueDesc,
    getIssuesList,
    Rule (..),
    Gravity(..),
    Issue (..),
    allRules,
    showLong,
    defaultRules,
    showArgo,
    showVera,
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

class (Show a) => ShowMode a where
  -- | Creates a vera compatible output of form:
  -- `<complete path>:<line>: <gravity>:<code>`
  showVera :: a -> String
  -- | Creates an Argo compatible output of form:
  -- `<complete path>:<line>:<code>`
  showArgo :: a -> String

instance ShowMode Warn where
  showVera = show
  showArgo (Warn w (f, l) _) = f ++ ":" ++ show l ++ ":" ++ showArgo w

instance ShowMode Issue where
  showVera i = let (idd, msg) = getIssueDesc i in idd ++ " # " ++ msg
  showArgo i = let (idd, _) = getIssueDesc i in idd


-- | Describes a coding style rule.
-- A rule can emit one or more 'Warn'.
data Rule = Rule { name :: String         -- ^ Rule name
                 , _description :: String -- ^ Rule description
                 , getRule :: Check       -- ^ Function to check the rule
                 }

instance Eq Rule where
  r1 == r2 = name r1 == name r2

showLong :: Warn -> String
showLong = show

