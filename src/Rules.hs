{-|
Coding style rules.
-}
module Rules (
    defaultRules,
) where

import BadDo
import BadDoReturn
import BadExtensionPragma
import BadGuard
import BadHeader
import BadIf
import FunctionTooWideOrLarge
import NoExportDecl
import NoModuleDecl
import NoSig
import Common

defaultRules :: [ParseSuccess -> [Warn]]
defaultRules = [ BadDo.check
               , BadDoReturn.check
               , BadExtensionPragma.check
               , BadGuard.check
               , BadHeader.check
               , BadIf.check
               , FunctionTooWideOrLarge.check
               , NoExportDecl.check
               , NoModuleDecl.check
               , NoSig.check ]
