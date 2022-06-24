{-|
A language pragma exists that enables an unauthorized extension.
-}
module BadExtensionPragma (
  check,
) where

import Common

import GHC.LanguageExtensions.Type
import Data.List

-- | Extensions to authorize.
-- Any other extensions will generate a 'BadExtensionPragma' warning.
authorizedExtensions :: [Extension]
authorizedExtensions = [TemplateHaskell]

-- | Check function.
-- After a successful parsing returns a list of zero or more 'Warn'.
check :: ParseSuccess -> [Warn]
check m = genWarn =<< (extensions <$> comments m)
  where
    extensions :: Located AnnotationComment -> [Located [Extension]]
    extensions a = languagePragmas $ blocks <$> a
    blocks :: AnnotationComment -> Maybe String
    blocks (AnnBlockComment s) = Just s
    blocks _ = Nothing

genWarn :: [Located [Extension]] -> [Warn]
genWarn pragmas = gen =<< pragmas
  where
    gen :: Located [Extension] -> [Warn]
    gen e = extensionListWarn e <> unauthorizedExtension e
    extensionListWarn :: Located [Extension] -> [Warn]
    extensionListWarn z@(L _ e)
      | length e > 1 = [mkWarn ForbiddenPragmaList (exL z) (StringArg $ fst $ exL z)]
      | otherwise = []
    unauthorizedExtension :: Located [Extension] -> [Warn]
    unauthorizedExtension z@(L _ [e])
      | e `notElem` authorizedExtensions = [mkWarn BadLangPragma (exL z) (DoubleStringArg (fst $ exL z) (show e))]
      | otherwise = []
    unauthorizedExtension _ = []

-- TODO : fix prototype should return Located [Extension] 1/pragma
-- TODO : Add the regex based match
languagePragmas :: Located (Maybe String) -> [Located [Extension]]
languagePragmas (L l (Just s)) = [L l [(getExt s)]]
languagePragmas _ = []

getExt :: String -> Extension
getExt s = case elemIndex s (show <$> enumFrom Cpp) of
            Nothing -> GADTs
            Just i -> toEnum i

