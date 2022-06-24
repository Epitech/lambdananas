{-|
Parser settings.
-}
module ParserSettings (
  parserFlags,
) where

import Lexer
import EnumSet
import Module
import GHC.LanguageExtensions
import DynFlags hiding (warningFlags)

-- | Warning flags to check for while parsing
warningFlags :: [WarningFlag]
warningFlags = []

-- | Flags to be given to the parser via 'mkPStatePure'
parserFlags :: ParserFlags
parserFlags = mkParserFlags'
  (fromList warningFlags)
  (fromList $ enumFrom Cpp)
  mainUnitId
  False
  False
  True
  False
