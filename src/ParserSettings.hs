-- Is required since we do not initialise the totality of 'Settings'

module ParserSettings (
  parserFlags,
) where

import Lexer
import EnumSet
import Module
import GHC.LanguageExtensions
import DynFlags hiding (warningFlags)

-- | Extensions to authorize while parsing
authorizedExtensions :: [Extension]
authorizedExtensions = [TemplateHaskell]

-- | Warning flags to check for while parsing
warningFlags :: [WarningFlag]
warningFlags = []

-- | Flags to be given to the parser via 'mkPStatePure'
parserFlags :: ParserFlags
parserFlags = mkParserFlags'
  (fromList warningFlags)
  (fromList authorizedExtensions)
  mainUnitId
  False
  False
  True
  False
