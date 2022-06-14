{-|
Function has no signature.
-}
module NoSig (
  check,
) where

import Common
import RdrName
import FastString
import OccName

check :: HsModule GhcPs -> [Warn]
check m = join $ genWarns <$> binds
  where
    binds = join $ collectBind <$> unL (hsmodDecls m)
    genWarns (RealSrcSpan l, e) = if elem e (join $ collectSigs <$> unL (hsmodDecls m))
      then []
      else [mkWarn NoSig (mF m, srcSpanStartLine l) (StringArg $ unpackFS e)]

collectSigs :: HsDecl GhcPs -> [FastString]
collectSigs (SigD _ t) = id' $ unL $ idList t
  where
    idList (TypeSig _ l _) = l
    idList _ = []
    id' [Unqual n] = [occNameFS n]
    id' _ = []
collectSigs _ = []

collectBind :: HsDecl GhcPs -> [(SrcSpan, FastString)]
collectBind (ValD _ t) = id' t
  where
    id' :: HsBind GhcPs -> [(SrcSpan, FastString)]
    id' FunBind {fun_id = L l (Unqual n)} = [(l, occNameFS n)]
    id' _ = []
collectBind _ = []
