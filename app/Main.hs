module Main where

import Parser
import Conf
import Control.Monad
import Data.List
import System.Directory
import System.FilePath.Posix
import Options.Applicative

main :: IO ()
main =  execParser options >>= processAll . translateConf where
    options = info (optParser <**> helper)
              (fullDesc
              <> header "Haskell Style Checker - An EPITECH Haskell Linter")

-- TODO : remove this glue code
-- | Translates a 'Conf'' to a 'Conf'.
translateConf :: Conf' -> Either String (Conf, [String])
translateConf (Conf' _ _ _ Nothing Nothing) = Left "No files or directories"
translateConf (Conf' _ _ _ (Just dir) (Just file)) = Right (Conf showLong defaultRules dir, file)
translateConf _ = Left "failed"

processAll :: Either String (Conf, [String]) -> IO ()
processAll (Left str) = putStrLn ("Error: "++str)
processAll (Right (conf,files)) = do
    files2 <- join <$> mapM loadDir (dirs conf)
    mapM_ (doOne conf) (files ++ files2)

doOne :: Conf -> String -> IO ()
doOne (Conf sFct rls _) filename = do
  buff <- parseFile filename
  case buff of
    Right lst -> let rs = map getRule rls
                     warnings = sort $ join $ map (\ f -> f lst) rs
                 in mapM_ (putStrLn . sFct) warnings
    Left err -> putStrLn $ "unable to load file: " ++ show (err :: IOError) -- TODO : check for extensions here by watching for the error returned

loadDir :: FilePath -> IO [FilePath]
loadDir dir = do
  files <- listDirectory dir
  files2 <- mapM (expandDir . (dir </>)) files
  return $ filter (\ f -> takeExtension f == ".hs" &&
                          takeFileName f /= "Setup.hs") $
    join files2

expandDir :: FilePath -> IO [FilePath]
expandDir f = doesDirectoryExist f >>=
                    \ isDir -> if isDir && ignore f
                               then loadDir f
                               else return [f]
  where ignore fl = takeFileName fl `notElem` ["tests", "test",
                                               "bonus",".stack-work"]
