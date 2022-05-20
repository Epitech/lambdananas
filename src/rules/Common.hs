module Common (
  Check,
  Warn (..),
  Gravity (..),
  Issue (..),
  module Language.Haskell.Exts.Syntax,
  module Language.Haskell.Exts.SrcLoc,
  module Control.Monad.Writer,
  module Parser,
) where

import Language.Haskell.Exts.Syntax hiding (Rule, Alt)
import Language.Haskell.Exts.SrcLoc
import Control.Monad.Writer
import Parser

type Check = [Decl SrcSpanInfo] -> [Warn]

-- | A coding style warning that can be emitted by a rule.
data Warn = Warn { what :: Issue                 -- ^ The issue raised
                 , _location :: (FilePath, Int)  -- ^ The location of the issue
                 , _gravity :: Gravity           -- ^ The gravity of the issue
                 } deriving Eq

-- | Describes an 'Issue' gravity.
data Gravity = Info | Minor | Major deriving Eq

-- | All possible style issues arising from a program.
data Issue = BadIf                -- ^ Nested ifs
           | BadDo                -- ^ Useless do
           | BadReturn            -- ^ Useless generator
           | BadGuard             -- ^ Guard should be a pattern
           | LineTooLong          -- ^ Line too long
           | FunctionTooBig       -- ^ Function too big
           | NoSig String         -- ^ No signature
           | NotParsable          -- ^ File is not parsable
           | ForbiddenExt         -- ^ File contains forbidden extension
           | Debug                -- ^ Debug
           deriving Eq

instance Show Issue where
  show i = let (idd, msg) = getIssueDesc i in idd ++ " # " ++ msg

instance Show Gravity where
  show Info = "INFO"
  show Minor = "MINOR"
  show Major = "MAJOR"

instance Show Warn where
  show (Warn w (f, l) g) = f ++ ":" ++ show l ++ ": " ++ show g ++ ":" ++ show w

instance Ord Warn where
  compare (Warn _ (s1,l1) _) (Warn _ (s2,l2) _) | s1 == s2 = compare l1 l2
                                            | otherwise = compare s1 s2
