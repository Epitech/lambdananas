{-|
Describes input operations on folders and files.
-}
module Files (
  load,
) where

import System.Directory
import System.FilePath.Posix
import Control.Monad

-- | A list of directories to be ignored.
ignoredDirs :: [String]
ignoredDirs = ["tests", "test", "bonus",".stack-work"]

-- | Given a 'FilePath' returns all haskell files found recursively.
-- example: `File.hs` gives `[File.hs]`
-- example: `src/` could give `[src/File1.hs, src/File2.hs]`
loadDir :: FilePath -> IO [FilePath]
loadDir dir = do
    files <- listDirectory dir
    files2 <- mapM (load . (dir </>)) files
    return $ filter (\ f -> takeExtension f == ".hs" &&
                          takeFileName f /= "Setup.hs") $ join files2

-- | If the given 'FilePath' is a directory, returns a list
-- of all files inside recursively. Given a file, will
-- return it as a single element list.
load :: FilePath -> IO [FilePath]
load f = doesDirectoryExist f >>=
                    \ isDir -> if isDir && ignore f
                               then loadDir f
                               else return [f]
  where ignore fl = takeFileName fl `notElem` ignoredDirs
