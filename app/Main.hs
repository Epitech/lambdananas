import Parser
import Conf
import System.Environment
import Control.Monad
import Data.List
import System.Directory
import System.FilePath.Posix

main :: IO ()
main = getArgs >>= processAll . doArgs defaultConf
  where processAll (Left "usage") = usage
        processAll (Left str) = putStrLn ("Error: "++str) >> usage
        processAll (Right (conf,files)) = do
          files2 <- join <$> mapM loadDir (dirs conf)
          mapM_ (doOne conf) (files ++ files2)

usage :: IO ()
usage = putStrLn (unwords ["usage: hsc [--short] [--long]",
                           "[--disable rule] [--enable rule]",
                           "[-d directory] [files]"])
        >> putStrLn "  - Rules:"
        >> mapM_ displayRule allRules
  where displayRule (Rule n desc _) =
          putStrLn ("    * "++ n ++":\n      "++ desc)

doOne :: Conf -> String -> IO ()
doOne (Conf sFct rls _) filename = do
  buff <- parseFile filename
  case buff of
    Right lst -> let rs = map getRule rls
                     warnings = sort $ join $ map (\ f -> f lst) rs
                 in mapM_ (putStrLn . sFct) warnings
    Left err -> putStrLn $ "unable to load file: "++ show (err :: IOError)
                  
loadDir :: FilePath -> IO [FilePath]
loadDir dir = do
  files <- listDirectory dir
  files2 <- mapM ((\ f -> doesDirectoryExist f >>=
                    \ isDir -> if isDir && f `notElem` ["test","bonus"]
                               then loadDir f
                               else return [f]) . (dir </>)) files
  return $ filter (\ f -> takeExtension f == ".hs" &&
                          takeFileName f /= "Setup.hs") $
    join files2
  
