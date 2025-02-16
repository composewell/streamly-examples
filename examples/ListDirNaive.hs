import Control.Monad
import System.Directory
import System.FilePath

listDir :: FilePath -> IO ()
listDir dir = do
    contents <- listDirectory dir
    let fullPaths = map (dir </>) contents
    forM_ fullPaths $ \path -> do
        isDir <- doesDirectoryExist path
        if isDir
        then do
            symlink <- pathIsSymbolicLink path
            if symlink
            then putStrLn path
            else listDir path -- depth first
        else putStrLn path
    putStrLn dir

main :: IO ()
main = listDir "."
