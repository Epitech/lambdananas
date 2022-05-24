{-|
Describes input operations on folders and files.
-}
module Input (
  load,
) where

import System.Directory
import System.FilePath.Posix
import Control.Monad

type ManualExclusions = [String]

-- | A list of directories to be ignored.
defaultExclusions :: [String]
defaultExclusions = ["tests", "test", "bonus", ".stack-work", ".git"]

-- | If the given 'FilePath' is a directory, returns a list
-- of all files inside recursively. Given a file, will
-- return it as a single element list.
load :: ManualExclusions -> FilePath -> IO [FilePath]
load excluded f = doesDirectoryExist f >>=
                    \ isDir -> if isDir && ignore f
                               then loadDir excluded f
                               else return [f]
  where ignore fl = takeFileName fl `notElem` defaultExclusions <> excluded

-- | Given a 'FilePath' returns all haskell files found recursively.
-- example: `File.hs` gives `[File.hs]`
-- example: `src/` could give `[src/File1.hs, src/File2.hs]`
loadDir :: ManualExclusions -> FilePath -> IO [FilePath]
loadDir excluded dir = do
    files <- listDirectory dir
    files2 <- mapM (load excluded . (dir </>)) files
    return $ filter (\ f -> takeExtension f == ".hs" &&
                          takeFileName f /= "Setup.hs") $ join files2

