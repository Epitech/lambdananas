{-|
Coding style rules.
-}
module Rules (
    defaultRules,
) where

import NoSig
import BadExtensionPragma
import BadHeader
import NoExportDecl
import NoModuleDecl
import Common

defaultRules :: [ParseSuccess -> [Warn]]
defaultRules = [ NoSig.check
               , BadExtensionPragma.check
               , NoExportDecl.check
               , NoModuleDecl.check
               , BadHeader.check ]
