{-|
Bad do.
-}
module BadDo (
  check,
) where

import Common

check :: ParseSuccess -> [Warn]
check _ = []
