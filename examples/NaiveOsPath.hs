{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Monad
import System.Directory.OsPath
import System.OsPath
import System.IO
import Data.Maybe

listDir :: OsPath -> IO ()
listDir dir = do
    contents <- listDirectory dir
    let fullPaths = map (dir </>) contents
    forM fullPaths $ \path -> do
        isDir <- doesDirectoryExist path
        if isDir
        then do
            symlink <- pathIsSymbolicLink path
            if symlink
            then putStrLn $ fromJust $ decodeUtf path
            else listDir path -- depth first
        else putStrLn $ fromJust $ decodeUtf path
    putStrLn $ fromJust $ decodeUtf dir

main :: IO ()
main = listDir [osp|.|]
