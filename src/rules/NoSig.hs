{-|
Function has no signature.
-}
module NoSig (
  check,
) where

import GHC.Hs hiding (NoSig)
import Control.Monad
import SrcLoc
import RdrName
import FastString
import OccName

import Warn

---- Support functions ----

-- | Removes location informations.
unLocate :: [Located a] -> [a]
unLocate l = extract <$> l
  where
    extract (L _ a) = a

check :: HsModule GhcPs -> [Warn]
check m = join $ collectSigs <$> unLocate (hsmodDecls m)

----------------------------

collectSigs :: HsDecl GhcPs -> [FastString]
collectSigs (SigD _ t) = name $ unLocate <$> t
  where
    name [Unqual n] = [occNameFS n]
    name _ = []
collectSigs _ = []
