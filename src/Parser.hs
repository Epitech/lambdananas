module Parser (
  parseFile,
  Node (..),
  explore,
  getLoc,
  inspectMatch,
  inspectExpr,
  getIdent,
  parseHS,
) where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Control.Exception
import Control.Monad (void)
--import Debug.Trace

data Node = NExp (Exp SrcSpanInfo)
          | NBin (Binds SrcSpanInfo)
          | NSmt (Stmt SrcSpanInfo)
          | NDec (Decl SrcSpanInfo)
          | NPat (Pat SrcSpanInfo)
          deriving (Eq, Show)

showS :: (Functor f, Show (f ())) => f a -> String
showS = show . void

parseFile :: String -> IO (Either IOError [Decl SrcSpanInfo])
parseFile filename = parseStr <$> try (readFile filename)
  where parseStr buff = buff >>= parseHS filename

parseHS :: String -> String -> Either IOError [Decl SrcSpanInfo]
parseHS file s =
  purge $ parseWithMode (defaultParseMode { parseFilename = file }) s
  where purge (ParseOk (Module _mod _ _ _ body)) = Right body
        purge err = Left (userError (show err))

getIdent :: Name SrcSpanInfo -> String
getIdent (Ident _ name) = name
getIdent (Symbol _ name) = name

getLoc :: SrcSpanInfo -> (String,Int)
getLoc (SrcSpanInfo (SrcSpan f l _ _ _) _) = (f,l)

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

inspectStmt :: Monoid a => (Node -> a) -> Stmt SrcSpanInfo -> a
inspectStmt f s@(Qualifier _ e) = f (NSmt s) <> inspectExpr f e
inspectStmt f s@(Generator _ _e1 e2) = f (NSmt s) <> inspectExpr f e2
inspectStmt f s@(LetStmt _ b) = f (NSmt s) <> inspectBinds f b
inspectStmt _ _ = mempty --trace (showS s) mempty

inspectBinds :: Monoid a => (Node -> a) -> Binds SrcSpanInfo -> a
inspectBinds f (BDecls _ lst) = foldMap (inspectDecl f) lst
inspectBinds _ _ = mempty --trace ("BINDS: "++showS b) mempty

inspectRhs :: Monoid a => (Node -> a) -> Rhs SrcSpanInfo -> a
inspectRhs f (UnGuardedRhs _ expr) = inspectExpr f expr
inspectRhs f (GuardedRhss _ exprs) = foldMap inspectRhs' exprs
  where inspectRhs' (GuardedRhs _ stm expr) = foldMap (inspectStmt f) stm <>
                                              inspectExpr f expr

inspectPat :: Monoid a => (Node -> a) -> Pat SrcSpanInfo -> a
inspectPat f p = f (NPat p) <> iPat p
  where iPat (PInfixApp _ p1 _ p2) = inspectPat f p1 <> inspectPat f p2
        iPat (PApp _ _ ps) = inspectPats f ps
        iPat (PParen _ pp) = inspectPat f pp
        iPat (PTuple _ _ ps) = inspectPats f ps
        iPat _ = mempty

inspectPats :: Monoid a => (Node -> a) -> [Pat SrcSpanInfo] -> a
inspectPats f = foldMap $ inspectPat f

inspectMatch :: Monoid a => (Node -> a) -> Match SrcSpanInfo -> a
inspectMatch f (Match _ _ ps a Nothing) = inspectPats f ps <>
                                          inspectRhs f a
inspectMatch f (Match _ _ ps a (Just l)) = inspectPats f ps <>
                                           inspectRhs f a <>
                                           inspectBinds f l
inspectMatch _ _ = mempty --trace (showS m) undefined

inspectDecl :: Monoid a => (Node -> a) -> Decl SrcSpanInfo -> a
inspectDecl f d@(PatBind _ (PVar _ _) a _) = f (NDec d) <> inspectRhs f a
inspectDecl f d@(FunBind _ lst) = f (NDec d) <> foldMap (inspectMatch f) lst
inspectDecl f d = f (NDec d) <> mempty

explore :: Monoid a => (Node -> a) -> [Decl SrcSpanInfo] -> [a]
explore f = map (inspectDecl f)

