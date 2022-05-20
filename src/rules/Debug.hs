{-|
This rule is used for debug only.
It's a bit legacy, but could be of use later.
-}
module Debug (
  description,
  hint,
  code,
  check,
) where

import Common

description :: Maybe [String] -> String
description (Just (s:_)) = s
description _ = "debug rule left empty"

hint :: String
hint = "debug rule found something, this should not happen!"

code :: String
code = "XX"

gravity :: Gravity
gravity = Info

check :: Check
check _ = [Warn (Debug "") ("", 0) gravity]
