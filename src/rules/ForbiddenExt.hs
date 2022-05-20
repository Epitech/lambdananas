{-|
Forbidden extensions.
-}
module ForbiddenExt (
  description,
  hint,
  code,
  check,
) where

import Common

description :: Maybe [String] -> String
description (Just (s:_)) = s ++ " contains forbidden extensions"
description _ = "a file contains forbidden extensions"

hint :: String
hint = "haskell language extensions are forbidden"

code :: String
code = "E1"

gravity :: Gravity
gravity = Major

check :: Check
check _ = [(Warn (ForbiddenExt "") ("", 0) gravity)]
