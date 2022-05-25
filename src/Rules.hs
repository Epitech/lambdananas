{-|
Coding style rules.
-}
module Rules (
    defaultRules,
) where

import BadIf
import NoSig
import BadDoReturn
import BadDo
import BadGuard
import FunctionTooWideOrLarge
import Common

defaultRules :: [Check]
defaultRules = [ NoSig.check, BadIf.check, BadDoReturn.check,
                 BadDo.check, BadGuard.check, FunctionTooWideOrLarge.check ]
