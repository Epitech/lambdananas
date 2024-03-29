{-
-- EPITECH PROJECT, 2023
-- Lambdananas
-- File description:
-- Common imports for all rules to use.
-}

module Common (
  Check,
  module Language.Haskell.Exts.Syntax,
  module Language.Haskell.Exts.SrcLoc,
  module Language.Haskell.Exts.Comments,
  module Control.Monad.Writer,
  module Parser,
  module Warn,
) where

import Language.Haskell.Exts.Syntax hiding (Rule, Alt)
import Language.Haskell.Exts.SrcLoc hiding (loc)
import Language.Haskell.Exts.Comments
import Control.Monad.Writer

import Parser
import Warn

type Check = PResult -> [Warn]
