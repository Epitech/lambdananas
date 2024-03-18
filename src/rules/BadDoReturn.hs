{-
-- EPITECH PROJECT, 2023
-- Lambdananas
-- File description:
-- Bad do return. 
-}

{-|
(Also named uselessReturn or uselessGenerator through the code).
-}

module BadDoReturn (
  check,
) where

import Common
import Control.Monad (join)

check :: Check
check presult = (join . explore checkReturn) (decls presult)
  where checkReturn (NExp (Do _ body)) =
          foldMap (badReturns . NSmt) body
        checkReturn _ = []
        badReturns = checkGen toWarn
        toWarn ssi True = [makeWarn BadReturn (getLoc ssi) NoArg]
        toWarn _ _ = []

checkGen :: Monoid m => (SrcSpanInfo -> Bool -> m) -> Node -> m
checkGen f (NSmt (Generator ssi _ e1)) = f ssi $ isReturn e1
checkGen _ _ = mempty

isReturn :: Exp SrcSpanInfo -> Bool
isReturn (App _ (Var _ (UnQual _ (Ident _ f))) _) = isRetOrPure f
isReturn (InfixApp _ (Var _ (UnQual _ (Ident _ f))) _ _) = isRetOrPure f
isReturn (Paren _ e) = isReturn e
isReturn _ = False

isRetOrPure :: String -> Bool
isRetOrPure "return" = True
isRetOrPure "pure" = True
isRetOrPure _ = False
