{-|
Forbidden extensions.
-}
module ForbiddenExt (
  check,
) where

import Common

check :: Check
check _ = [(Warn (ForbiddenExt "") ("", 0) gravity)]
