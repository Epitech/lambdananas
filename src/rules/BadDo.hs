{-|
Bad do.
-}
module BadDo (
  description,
  hint,
  code,
  check,
) where

import Common

description :: Maybe [String] -> String
description _ = "useless do"

hint :: String
hint = "do structures should be avoided when no generator are used"

code :: String
code = "D1"

gravity :: Gravity
gravity = Major

check :: Check
check = join . explore checkDo
  where checkDo (NExp (Do ssi body)) | countGenerators body < 1 =
                                       [Warn BadDo (getLoc ssi) gravity]
        checkDo _ = []
        countGenerators = foldMap (countGenerator . NSmt)
        countGenerator :: Node -> Sum Int
        countGenerator = checkGen (\ _ isRet ->
                                     if isRet then Sum 0 else Sum 1)

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