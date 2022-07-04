{-|
A language pragma exists that enables an unauthorized extension.
-}
module BadExtensionPragma (
  check,
) where

import Common

import GHC.LanguageExtensions.Type
import Data.List
import Data.Maybe
import Text.Regex.TDFA

-- | Extensions to authorize.
-- Any other extensions will generate a 'BadExtensionPragma' warning.
authorizedExtensions :: [Extension]
authorizedExtensions = [ LambdaCase ]

-- | Check function.
-- After a successful parsing returns a list of zero or more 'Warn'.
check :: ParseSuccess -> [Warn]
check m = genWarn =<< (blockComments <$> comments m)
  where
    blockComments :: Located AnnotationComment -> Located (Maybe [Extension])
    blockComments a = languagePragma $ blocks <$> a
    blocks :: AnnotationComment -> Maybe String
    blocks (AnnBlockComment s) = Just s
    blocks _ = Nothing

-- TODO : fix nothing abuses
-- | Generate a warning given a list of extensions or nothing if parsing failed.
genWarn :: Located (Maybe [Extension]) -> [Warn]
genWarn p = extensionListWarn p <> unauthorizedExtension p

-- | Parses a list of pragmas as 'String' to 'Extension'.
languagePragma :: Located (Maybe String) -> Located (Maybe [Extension])
languagePragma s = parse <$> s
  where
    parse :: Maybe String -> Maybe [Extension]
    parse a = fromMaybe Nothing (foldr fun (Just []) <$> (parsePragma <$> a))
    fun :: Maybe Extension -> Maybe [Extension] -> Maybe [Extension]
    fun (Just e) l = (e:) <$> l
    fun Nothing Nothing = Nothing
    fun Nothing _ = Nothing

-- | Reports a 'ForbiddenPragmaList' when more than one extension is found in a LANGUAGE pragma.
extensionListWarn :: Located (Maybe [Extension]) -> [Warn]
extensionListWarn z@(L _ (Just e))
  | length e > 1 =
    [mkWarn ForbiddenPragmaList (exL z) (StringArg $ fst $ exL z)]
  | otherwise =
    []
extensionListWarn _ = []

-- | Reports a 'BadLangPragma' when a forbidden extension is used.
unauthorizedExtension :: Located (Maybe [Extension]) -> [Warn]
unauthorizedExtension z@(L _ (Just [e]))
  | e `notElem` authorizedExtensions =
    [mkWarn BadLangPragma (exL z) (DoubleStringArg (fst $ exL z) (show e))]
  | otherwise =
    []
unauthorizedExtension _ = []

-- | Parses a LANGUAGE pragma 'String' to a list of 'Extension'.
-- If the parsing fails, returns 'Nothing'.
parsePragma :: String -> [Maybe Extension]
parsePragma s
  | s =~ "{-# *LANGUAGE (([A-Za-z])+ *,? *)+ *#-}" =
    toExt <$> filter removeLang (getAllTextMatches (s =~ "[a-zA-Z]+") :: [String])
  | otherwise =
    []
  where
    toExt :: String -> Maybe Extension
    toExt e = e `elemIndex` (show <$> enumFrom Cpp) >>= Just . toEnum
    removeLang :: String -> Bool
    removeLang "LANGUAGE" = False
    removeLang _ = True
