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
    extensions :: Located AnnotationComment -> Located (Maybe [Extension])
    extensions a = languagePragmas $ blocks <$> a
    blocks :: AnnotationComment -> Maybe String
    blocks (AnnBlockComment s) = Just s
    blocks _ = Nothing

genWarn :: Located (Maybe [Extension]) -> [Warn]
genWarn p = extensionListWarn p <> unauthorizedExtension p

extensionListWarn :: Located (Maybe [Extension]) -> [Warn]
extensionListWarn z@(L _ (Just e))
  | length e > 1 =
    [mkWarn ForbiddenPragmaList (exL z) (StringArg $ fst $ exL z)]
  | otherwise =
    []
extensionListWarn _ = []

unauthorizedExtension :: Located (Maybe [Extension]) -> [Warn]
unauthorizedExtension z@(L _ (Just [e]))
  | e `notElem` authorizedExtensions =
    [mkWarn BadLangPragma (exL z) (DoubleStringArg (fst $ exL z) (show e))]
  | otherwise =
    []
unauthorizedExtension _ = []

-- TODO : Add the regex based match
languagePragmas :: Located (Maybe String) -> Located (Maybe [Extension])
languagePragmas s = parse <$> s
  where
    parse :: Maybe String -> Maybe [Extension]
    parse (Just _) = Just [GADTs]
    parse Nothing = Nothing
