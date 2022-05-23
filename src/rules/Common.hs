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
