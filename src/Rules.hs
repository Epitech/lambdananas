module Rules where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Parser
import Control.Monad
import Control.Monad.Writer
import Debug.Trace

type Check = [Decl SrcSpanInfo] -> [Warn]

data Issue = NoSig String
           | BadIf
           | BadReturn
           | BadGuard
           | Debug String
           | BadDo
           deriving Eq

instance Show Issue where
  show (NoSig s) = "T1 # " ++ s ++ " has no signature" -- T types
  show BadIf     = "C1 # nested IFs" -- C conditonnal branching
  show BadReturn = "D2 # useless generator"  -- D do and generators
  show BadGuard  = "C2 # guard should be a pattern" -- C conditional branch.
  show BadDo     = "D1 # useless DO"
  show (Debug s) = "DBG: "++s

data Warn = Warn { _what :: Issue
                 , _location :: (String, Int)
                 } deriving Eq

instance Show Warn where
  show (Warn w (f, l)) = f ++ ":" ++ show l ++ ":" ++ show w

instance Ord Warn where
  compare (Warn _ (s1,l1)) (Warn _ (s2,l2)) | s1 == s2 = compare l1 l2
                                            | otherwise = compare s1 s2

{- CHECK CASCADING IFS -}
checkIfs :: Check
checkIfs = join . explore checkIf
  where checkIf (NExp (If ssi _ ift ife)) | countIfs ift ife >= 1 =
                                                  [Warn BadIf (getLoc ssi)]
        checkIf _ = []                                                  
        countIfs ifthen ifelse = inspectExpr countIf ifthen <>
                                 inspectExpr countIf ifelse
        countIf (NExp If{}) = Sum 1
        countIf _ = Sum 0

{- CHECK USELESS DOs -}
checkDos :: Check
checkDos = join . explore checkDo
  where checkDo (NExp (Do ssi body)) | countGenerators body < 1 =
                                       [Warn BadDo (getLoc ssi)]
        checkDo _ = []
        countGenerators = foldMap (countGenerator . NSmt)
        countGenerator = checkGen (\ _ isRet ->
                                     if isRet then Sum 0 else Sum 1)

{- CHECK USELESS Returns -}
checkReturns :: Check
checkReturns = join . explore checkReturn
  where checkReturn (NExp (Do _ body)) =
          foldMap (badReturns . NSmt) body
        checkReturn _ = []
        badReturns = checkGen toWarn
        toWarn ssi True = [Warn BadReturn (getLoc ssi)]
        toWarn _ _ = []

{- auxiliary functions for checkDos and checkReturns -}
checkGen :: Monoid m => (SrcSpanInfo -> Bool -> m) -> Node -> m
checkGen f (NSmt (Generator ssi _ e1)) = f ssi $ isReturn e1
checkGen f _ = mempty

isReturn :: Exp SrcSpanInfo -> Bool
isReturn (App _ (Var _ (UnQual _ (Ident _ f))) _) = isRetOrPure f
isReturn (InfixApp _ (Var _ (UnQual _ (Ident _ f))) _ _) = isRetOrPure f
isReturn (Paren _ e) = isReturn e
isReturn _ = False

isRetOrPure :: String -> Bool
isRetOrPure "return" = True
isRetOrPure "pure" = True
isRetOrPure _ = False

{- CHECK IF ALL TOP DECLARATION HAS A TYPE SIGNATURE -}
checkSigs :: Check
checkSigs lst = join $ map genWarn binds
  where sigsAndBinds = explore collect lst
        sigs = foldl (<>) mempty $ map fst sigsAndBinds
        binds = foldl (<>) mempty $ map getBind sigsAndBinds
        getBind (_,l) = if l == [] then [] else [head l]
        genWarn (fct, ssi) | fct `notElem` sigs =
                             [Warn (NoSig fct) (getLoc ssi)]
        genWarn _ = []
        collect (NDec (TypeSig ssi (x:_) t )) = ([getIdent x],[])
        collect (NDec (PatBind ssi (PVar _ idt) _ _ )) =
          ([],[((getIdent idt), ssi)])
        collect (NDec (FunBind ssi (Match _ idt _ _ _:_))) =
          ([],[((getIdent idt), ssi)])
        collect _ = ([],[])

{- CHECK BAD GUARDS -}
checkGuards :: Check
checkGuards lst = join $ explore checkGuard lst
  where
    checkGuard (NDec (FunBind _ match)) =
      let vars = map (inspectMatch collectVar) match
          matchs = map (inspectMatch collectGuards) match
      in foldMap id $ zipWith toWarns vars matchs
    checkGuard _ = []

    collectVar (NPat (PVar ssi idt)) = [getIdent idt]
    collectVar _ = []
    
    collectGuards (NSmt (Qualifier _ expr)) = [expr]
    collectGuards _ = []
    
    toWarns :: [String] -> [Exp SrcSpanInfo] -> [Warn]
    toWarns vars lst = foldMap (inspectExpr (toWarn vars)) lst
    toWarn vars (NExp (InfixApp ssi e1 e2 e3))
      | isBadGuard vars e1 e2 e3 = [Warn BadGuard (getLoc ssi)]
    toWarn _ _ = []

    isBadGuard vars e1 e2 e3 | isVar vars e1 && isEq e2 && isLit e3 = True
                             | isLit e1 && isEq e2 && isVar vars e3 = True
    isBadGuard _ _ _ _ = False

    isVar vars (Var _ (UnQual _ (Ident _ x))) | x `elem` vars = True
    isVar _ _ = False

    isEq (QVarOp _ (UnQual _ (Symbol _ "=="))) = True
    isEq _ = False

    isLit Lit{} = True
    isLit Con{} = True
    isLit (App _ Con{} _) = True
    isLit (List _ []) = True
    isLit e = False
