{-|
Bad guard.
-}
module BadGuard (
  check,
) where

import Common

check :: ParseSuccess -> [Warn]
check m = []
