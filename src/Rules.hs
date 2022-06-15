{-|
Coding style rules.
-}
module Rules (
    defaultRules,
) where

import NoSig
import Common

defaultRules :: [HsModule GhcPs -> [Warn]]
defaultRules = [ NoSig.check ]
