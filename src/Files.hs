module Files (
  loadDir,
  expandDir,
) where

import System.Directory
import System.FilePath.Posix
import Control.Monad

-- TODO : Document
loadDir :: FilePath -> IO [FilePath]
loadDir dir = do
    files <- listDirectory dir -- files = ["path"]
    files2 <- mapM (expandDir . (dir </>)) files --["path/path"]
    return $ filter (\ f -> takeExtension f == ".hs" &&
                          takeFileName f /= "Setup.hs") $ join files2

-- TODO : Document
expandDir :: FilePath -> IO [FilePath]
expandDir f = doesDirectoryExist f >>=
                    \ isDir -> if isDir && ignore f
                               then loadDir f
                               else return [f]
  where ignore fl = takeFileName fl `notElem` ["tests", "test",
                                               "bonus",".stack-work"]
