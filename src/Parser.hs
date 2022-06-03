{-|
Parsing wrapper and inspection functions.
-}
module Parser (
  parseFile,
  Node (..),
  explore,
  getLoc,
  inspectMatch,
  inspectExpr,
  getIdent,
  parseHS,
  ParseError (..),
) where

import Common (Literal(String))

data Node = NExp (Exp SrcSpanInfo)
          | NBin (Binds SrcSpanInfo)
          | NSmt (Stmt SrcSpanInfo)
          | NDec (Decl SrcSpanInfo)
          | NPat (Pat SrcSpanInfo)
          deriving (Eq, Show)

data ParseError = ParseError { file :: String
                             , line :: Int
                             , column :: Int
                             , desc :: String
                             } deriving Eq

instance Show ParseError where
  show (ParseError f l c d) = d ++ ' ':f ++ ' ':show l ++ ':':show c

-- | Parse a file.
-- Basically a wrapper around 'parseHS'.
parseFile :: FilePath -- ^ File to be parsed
          -> IO (Either ParseError [Decl SrcSpanInfo]) -- ^ An error or a list of top level declarations
parseFile filename = do
    s <- readFile filename
    return $ parseHS filename s


-- | Parse a string.
parseHS :: FilePath -- ^ Name of the parsed file
        -> String -- ^ Content of the parsed file
        -> Either ParseError [Decl SrcSpanInfo]
parseHS filename s =
    purge $ parseWithMode (defaultParseMode { parseFilename = filename }) s
  where purge (ParseOk (Module _mod _ _ _ body)) = Right body
        purge (ParseFailed (SrcLoc f l c) err) = Left $ ParseError f l c err
        purge _ = Left $ ParseError "?" 0 0 "this should never happen"

-- | Gets a identifier from a 'Name'.
getIdent :: Name SrcSpanInfo -> String
getIdent (Ident _ name) = name
getIdent (Symbol _ name) = name

-- | Gets a localisation from a 'SrcSpanInfo'.
getLoc :: SrcSpanInfo
       -> (String,Int) -- ^ Filename and line
getLoc (SrcSpanInfo (SrcSpan f l _ _ _) _) = (f,l)

-- looks like a map
explore :: Monoid a => (Node -> a) -> [Decl SrcSpanInfo] -> [a]
explore f = map (inspectDecl f)

inspectDecl :: Monoid a => (Node -> a) -> Decl SrcSpanInfo -> a
inspectDecl f d@(PatBind _ (PVar _ _) a _) = f (NDec d) <> inspectRhs f a
inspectDecl f d@(FunBind _ lst) = f (NDec d) <> foldMap (inspectMatch f) lst
inspectDecl f d = f (NDec d) <> mempty

inspectRhs :: Monoid a => (Node -> a) -> Rhs SrcSpanInfo -> a
inspectRhs f (UnGuardedRhs _ expr) = inspectExpr f expr
inspectRhs f (GuardedRhss _ exprs) = foldMap inspectRhs' exprs
  where inspectRhs' (GuardedRhs _ stm expr) = foldMap (inspectStmt f) stm <>
                                              inspectExpr f expr

inspectStmt :: Monoid a => (Node -> a) -> Stmt SrcSpanInfo -> a
inspectStmt f s@(Qualifier _ e) = f (NSmt s) <> inspectExpr f e
inspectStmt f s@(Generator _ _e1 e2) = f (NSmt s) <> inspectExpr f e2
inspectStmt f s@(LetStmt _ b) = f (NSmt s) <> inspectBinds f b
inspectStmt _ _ = mempty --trace (showS s) mempty

inspectMatch :: Monoid a => (Node -> a) -> Match SrcSpanInfo -> a
inspectMatch f (Match _ _ ps a Nothing) = inspectPats f ps <>
                                          inspectRhs f a
inspectMatch f (Match _ _ ps a (Just l)) = inspectPats f ps <>
                                           inspectRhs f a <>
                                           inspectBinds f l
inspectMatch _ _ = mempty --trace (showS m) undefined

inspectPats :: Monoid a => (Node -> a) -> [Pat SrcSpanInfo] -> a
inspectPats f = foldMap $ inspectPat f

inspectPat :: Monoid a => (Node -> a) -> Pat SrcSpanInfo -> a
inspectPat f p = f (NPat p) <> iPat p
  where iPat (PInfixApp _ p1 _ p2) = inspectPat f p1 <> inspectPat f p2
        iPat (PApp _ _ ps) = inspectPats f ps
        iPat (PParen _ pp) = inspectPat f pp
        iPat (PTuple _ _ ps) = inspectPats f ps
        iPat _ = mempty

inspectExpr :: Monoid a => (Node -> a) -> Exp SrcSpanInfo -> a
inspectExpr f e0@(InfixApp _ e1 _q e2) = f (NExp e0) `mappend`
                                        inspectExpr f e1 `mappend`
                                        inspectExpr f e2
inspectExpr f e0@(App _ e1 e2) = f (NExp e0) `mappend`
                                 inspectExpr f e1 `mappend`
                                 inspectExpr f e2
inspectExpr f e0@(NegApp _ e1) = f (NExp e0) `mappend` inspectExpr f e1
inspectExpr f e0@(Lambda _ _pat e1) = f (NExp e0) `mappend` inspectExpr f e1
inspectExpr f e0@(Let _ binds e1) = inspectBinds f binds `mappend`
                                    f (NExp e0) `mappend`
                                    inspectExpr f e1
inspectExpr f e0@(If _ e1 e2 e3) = f (NExp e0) `mappend` inspectExpr f e1
                                   `mappend` inspectExpr f e2
                                   `mappend` inspectExpr f e3
inspectExpr f (Paren _ e) = inspectExpr f e
inspectExpr f e0@(Do _ body) = f (NExp e0) `mappend`
  foldMap (inspectStmt f) body
inspectExpr f e0@(Case _ e1 alts) =
  f (NExp e0) `mappend` inspectExpr f e1 `mappend`
  foldMap (inspectAlt f) alts
inspectExpr f e0 = f (NExp e0)

inspectAlt :: Monoid a => (Node -> a) -> Alt SrcSpanInfo -> a
inspectAlt f (Alt _ pat rhs Nothing) = inspectPat f pat <> inspectRhs f rhs
inspectAlt f (Alt _ pat rhs (Just binds)) = inspectPat f pat <>
                                            inspectRhs f rhs <>
                                            inspectBinds f binds

inspectBinds :: Monoid a => (Node -> a) -> Binds SrcSpanInfo -> a
inspectBinds f (BDecls _ lst) = foldMap (inspectDecl f) lst
inspectBinds _ _ = mempty --trace ("BINDS: "++showS b) mempty
                                              inspectExpr f expr
