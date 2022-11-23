{-
-- EPITECH PROJECT, 2022
-- Lambdananas
-- File description:
-- Describes input operations on folders and files.
-}

module Input (
  load,
) where

import System.Directory
import System.FilePath.Posix
import Control.Monad

-- | The list of directories to be ignored by default.
defaultExclusions :: [String]
defaultExclusions = ["tests", "test", "bonus", ".stack-work", ".git"]

-- | Just to make functions clearer.
type ManualExclusions = [FilePath]

-- | If the given 'FilePath' is a directory, returns a list
-- of all files inside recursively. Given a file, will
-- return it as a single element list.
load :: ManualExclusions -> FilePath -> IO [FilePath]
load excluded filename = doesDirectoryExist filename >>=
                    \ isDir -> if isDir && not (isExcluded excluded filename)
                               then loadDir excluded filename
                               else return [filename]
  where
    isExcluded [] f =  takeFileName f `elem` defaultExclusions
    isExcluded ex f =  takeFileName f `elem` ex

-- | Given a 'FilePath' returns all haskell files found recursively.
-- example: `File.hs` gives `[File.hs]`
-- example: `src/` could give `[src/File1.hs, src/File2.hs]`
loadDir :: ManualExclusions -> FilePath -> IO [FilePath]
loadDir excluded dir = do
    files <- listDirectory dir
    files2 <- mapM (load excluded . (dir </>)) files
    return $ filter (\ f -> takeExtension f == ".hs" &&
                          takeFileName f /= "Setup.hs") $ join files2

