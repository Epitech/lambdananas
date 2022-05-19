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

-- | A coding style warning emitted by the checker.
data Warn = Warn { what :: Issue                -- ^ The issue raised (description can be retrived by 'getIssueDesc')
                 , _location :: (FilePath, Int) -- ^ The location of the issue
                 , gravity :: Gravity           -- ^ The gravity of the issue
                 } deriving Eq

-- | Describes an 'Issue' gravity.
data Gravity = Info | Minor | Major deriving Eq

-- | All possible style issues arising from a program.
data Issue = BadIf                        -- ^ Nested ifs
           | BadDo                        -- ^ Useless do
           | BadReturn                    -- ^ Useless generator
           | BadGuard                     -- ^ Guard should be a pattern
           | LineTooLong                  -- ^ Line too long
           | FunctionTooBig               -- ^ Function too big
           | NoSig String                 -- ^ No signature
           | NotParsable FilePath         -- ^ File is not parsable
           | ForbiddenExt FilePath        -- ^ File contains forbidden extension
           | Debug String                 -- ^ Debug
           deriving Eq
