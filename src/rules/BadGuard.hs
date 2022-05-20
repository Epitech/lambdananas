{-|
Bad guard.
-}
module BadGuard (
  description,
  hint,
  code,
  check,
) where

import Common
import Data.Foldable

description :: Maybe [String] -> String
description _ = "guard should be a pattern"

hint :: String
hint = "guard constructs should only be used if it cannot be a pattern match"

code :: String
code = "C2"

gravity :: Gravity
gravity = Major

check :: Check
check lst = join $ explore checkGuard lst
  where checkGuard (NDec (FunBind _ m)) =
          fold $ zipWith toWarns (vars m) (matchs m)
        checkGuard _ = []
        vars match = map (inspectMatch collectVar) match
        matchs match = map (inspectMatch collectGuards) match
        collectVar (NPat (PVar _ idt)) = [getIdent idt]
        collectVar _ = []
        collectGuards (NSmt (Qualifier _ expr)) = [expr]
        collectGuards _ = []

toWarns :: [String] -> [Exp SrcSpanInfo] -> [Warn]
toWarns vars = foldMap (inspectExpr toWarn)
  where toWarn (NExp (InfixApp ssi e1 e2 e3))
          | isBadGuard vars e1 e2 e3 = [Warn BadGuard (getLoc ssi) gravity]
        toWarn _ = []

isBadGuard :: [String] -> Exp SrcSpanInfo -> QOp SrcSpanInfo ->
              Exp SrcSpanInfo -> Bool
isBadGuard vars e1 e2 e3 | isVar e1 && isEq e2 && isLit e3 = True
                         | isLit e1 && isEq e2 && isVar e3 = True
  where isVar (Var _ (UnQual _ (Ident _ x))) | x `elem` vars = True
        isVar _ = False
        isEq (QVarOp _ (UnQual _ (Symbol _ "=="))) = True
        isEq _ = False
isBadGuard _ _ _ _ = False

isLit :: Exp SrcSpanInfo -> Bool
isLit Lit{} = True
isLit Con{} = True
isLit (App _ Con{} _) = True
isLit (List _ []) = True
isLit _ = False
