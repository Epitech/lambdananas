{-|
Bad do return.
(Also named uselessReturn or uselessGenerator through the code).
-}
module BadDoReturn (
  check,
) where

import Common

check :: ParseSuccess -> [Warn]
check _ = []
