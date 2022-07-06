module NoModuleDecl where

import Common

check :: ParseSuccess -> [Warn]
check m = case hsmodName $ pt m of
  Nothing -> [mkWarn NoModuleDecl (mF $ pt m, 1) (StringArg $ mF $ pt m)]
  Just _ -> []
