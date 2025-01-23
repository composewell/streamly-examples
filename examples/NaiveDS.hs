{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

-- import System.OsPath
import Data.Maybe
import System.OsPath.Posix
import System.Posix.Directory.PosixPath
import NaiveDirStream

main :: IO ()
-- main = openDirStream [osp|.|] >>= loopDir
main =
    let name = [pstr|.|]
    in do
        putStrLn $ fromJust $ decodeUtf name
        openDirStream name >>= loopDir name
