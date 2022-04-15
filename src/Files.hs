{-|
Describes input operations on folders and files.
-}
module Files (
  loadDir,
) where

import System.Directory
import System.FilePath.Posix
import Control.Monad

-- | Gets a list of all files recursively inside a directory.
loadDir :: FilePath -> IO [FilePath]
loadDir dir = do
    files <- listDirectory dir
    files2 <- mapM (expandDir . (dir </>)) files
    return $ filter (\ f -> takeExtension f == ".hs" &&
                          takeFileName f /= "Setup.hs") $ join files2

-- | A list of directories to be ignored.
ignoredDirs :: [String]
ignoredDirs = ["tests", "test", "bonus",".stack-work"]

-- | If the given 'FilePath' is a directory, returns a list
-- of all files inside recursively. Given a file, will
-- return it as a single element list.
expandDir :: FilePath -> IO [FilePath]
expandDir f = doesDirectoryExist f >>=
                    \ isDir -> if isDir && ignore f
                               then loadDir f
                               else return [f]
  where ignore fl = takeFileName fl `notElem` ignoredDirs
