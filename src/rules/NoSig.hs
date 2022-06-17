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

check :: ParseSuccess -> [Warn]
check m = genWarns =<< binds
  where
    binds = collectBind =<< unL (hsmodDecls $ pt m)
    genWarns (RealSrcSpan l, e) = if e `elem` (collectSigs =<< unL (hsmodDecls $ pt m))
      then []
      else [mkWarn NoSig (mF $ pt m, srcSpanStartLine l) (StringArg $ unpackFS e)]
    genWarns _ = [mkWarn Debug (mF $ pt m, 0) (StringArg "Failure")]

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
