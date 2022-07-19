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
import BadHeader
import Common

-- | Every rule here will be used when lambdananas is executed.
defaultRules :: [Check]
defaultRules = [ NoSig.check, BadIf.check, BadDoReturn.check,
                 BadDo.check, BadGuard.check, FunctionTooWideOrLarge.check,
                 BadHeader.check ]
