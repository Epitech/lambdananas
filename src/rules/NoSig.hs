{-|
Function has no signature.
-}
module NoSig (
  description,
  hint,
  code,
  check,
) where

import Common

description :: Maybe [String] -> String
description (Just (s:_)) = s ++ " has no signature"
description _ = "function has no signature"

hint :: String
hint = "top-level declaration has no corresponging type signature"

code :: String
code = "T1"

gravity :: Gravity
gravity = Minor

check :: Check
check lst = join $ map genWarn binds
  where sigsAndBinds = explore collectSigs lst
        sigs = foldMap fst sigsAndBinds
        binds = foldMap getBind sigsAndBinds
        getBind (_,l) = if null l then [] else [head l]
        genWarn (fct, ssi) | fct `notElem` sigs =
                             [Warn (NoSig fct) (getLoc ssi) gravity]
        genWarn _ = []

collectSigs :: Node -> ([String], [(String, SrcSpanInfo)])
collectSigs (NDec (TypeSig _ (x:_) _ )) = ([getIdent x],[])
collectSigs (NDec (PatBind ssi (PVar _ idt) _ _ )) =
  ([],[(getIdent idt, ssi)])
collectSigs (NDec (FunBind ssi (Match _ idt _ _ _:_))) =
  ([],[(getIdent idt, ssi)])
collectSigs _ = ([],[])
