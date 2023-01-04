{-
-- EPITECH PROJECT, 2023
-- Lambdananas
-- File description:
-- Function has no signature.
-}

module NoSig (
  check,
) where

import Common

check :: Check
check presult = join $ map genWarn binds
  where sigsAndBinds = explore collectSigs (decls presult)
        sigs = foldMap fst sigsAndBinds
        binds = foldMap getBind sigsAndBinds
        getBind (_,l) = if null l then [] else [head l]
        genWarn (fct, ssi) | fct `notElem` sigs =
                              [makeWarn NoSig (getLoc ssi) $ StringArg fct]
        genWarn _ = []

collectSigs :: Node -> ([String], [(String, SrcSpanInfo)])
collectSigs (NDec (TypeSig _ (x:_) _ )) = ([getIdent x],[])
collectSigs (NDec (PatBind ssi (PVar _ idt) _ _ )) =
  ([],[(getIdent idt, ssi)])
collectSigs (NDec (FunBind ssi (Match _ idt _ _ _:_))) =
  ([],[(getIdent idt, ssi)])
collectSigs _ = ([],[])
