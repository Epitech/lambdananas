{-|
Coding style rules.
-}
module Rules (
    defaultRules,
) where

import NoSig
import BadExtensionPragma
import Common

defaultRules :: [ParseSuccess -> [Warn]]
defaultRules = [ NoSig.check, BadExtensionPragma.check ]
