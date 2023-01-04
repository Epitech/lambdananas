{-
-- EPITECH PROJECT, 2023
-- Lambdananas
-- File description:
-- Forbidden imports
-}

module ForbiddenImports (
  check,
) where

import Common

forbiddenImportList :: [String]
forbiddenImportList = [ "Data.IORef"
                      , "Data.STRef"
                      , "Control.Concurrent.STM.TVar"
                      , "System.IO.Unsafe"
                      ]

check :: Check
check presult = map toWarn $ getForbiddenImports (imports presult)
  where
    getForbiddenImports lst = filter filterImp $ map getImp lst
    getImp (ImportDecl ssi (ModuleName _ name) _ _ _ _ _ _) = (name, ssi)
    filterImp (name, _) = name `elem` forbiddenImportList
    toWarn (name, ssi) =
      makeWarn ForbiddenImports (getLoc ssi) (StringArg name)
