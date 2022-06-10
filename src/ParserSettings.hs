{-# OPTIONS_GHC -Wno-missing-fields#-}
-- Is required since we do not initialise the totality of 'Settings'

module ParserSettings (
  settings,
) where

import Settings
import ToolSettings
import FileSettings
import GhcNameVersion
import PlatformConstants
import Fingerprint
import GHC.Platform
import GHC.Version

-- | A Settings structure used to create dynflags.
-- This is copied from the miniHlint example.
-- See : https://github.com/digital-asset/ghc-lib/blob/master/examples/mini-hlint/src/Main.hs
settings :: Settings
settings = Settings
  { sGhcNameVersion = ghcNameVersion
  , sFileSettings = fileSettings
  , sTargetPlatform = platform
  , sPlatformMisc = platformMisc
  , sPlatformConstants = platformConstants
  , sToolSettings = toolSettings
  , sRawSettings = []
  }
  where
    toolSettings = ToolSettings {
      toolSettings_opt_P_fingerprint=fingerprint0
      }
    fileSettings = FileSettings {}
    platformMisc = PlatformMisc {}
    ghcNameVersion = GhcNameVersion {
      ghcNameVersion_programName="ghc",
      ghcNameVersion_projectVersion=cProjectVersion
    }
    platform = Platform {
      platformWordSize = PW8,
      platformMini = PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown},
      platformUnregisterised=True
    }
    platformConstants = PlatformConstants {
          pc_DYNAMIC_BY_DEFAULT = False,
          pc_WORD_SIZE = 8
    }



