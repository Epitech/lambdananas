module NoExportDecl where

import Common
import Module

check :: ParseSuccess -> [Warn]
check a = case hsmodExports m of
    Nothing -> if moduleIsNotMain m
               then [Warn NoExportDecl (mF m, 1) (StringArg $ mF m)]
               else []
    _ -> []
  where
    m = pt a

moduleIsNotMain :: HsModule GhcPs -> Bool
moduleIsNotMain a = case hsmodName a of
  Nothing -> True
  Just b ->  moduleNameString (unLoc b) /= "Main"
