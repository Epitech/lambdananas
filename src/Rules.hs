{-|
Coding style rules.
-}
module Rules (
    defaultRules,
) where

import NoSig
import Common

defaultRules :: [ParseSuccess -> [Warn]]
defaultRules = [ NoSig.check ]
