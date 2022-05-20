{-|
Not parsable.
-}
module NotParsable (
  description,
  hint,
  code,
  check,
) where

import Common

description :: Maybe [String] -> String
description (Just (s:_)) = s ++ " could not be parsed"
description _ = "a file could not be parsed"

hint :: String
hint = "all files must be parsable without extensions"

code :: String
code = "P1"

check :: Check
check _ = []
