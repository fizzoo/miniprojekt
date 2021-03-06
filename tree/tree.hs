import System.Directory
import System.Environment
import Control.Monad

isParent :: FilePath -> Bool
isParent file = file == "." || file == ".."

recDir ::  FilePath -> IO ()
recDir path = do
    files <- getDirectoryContents path
    let filteredfiles = filter (not . isParent) files
        filteredpaths = map ((path ++ "/") ++) filteredfiles
    fdirs <- filterM doesDirectoryExist filteredpaths
    ffiles <- filterM doesFileExist filteredpaths
    mapM_ putStrLn ffiles
    mapM_ recDir fdirs

main :: IO ()
main = do
    args <- getArgs
    dir <- case args of
          [str] -> canonicalizePath str
          _ -> getCurrentDirectory
    recDir dir
