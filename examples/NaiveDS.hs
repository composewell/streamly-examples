{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Maybe (fromJust)
import NaiveDirStream (loopDir)

import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.Posix.ReadDir as ReadDir

main :: IO ()
main = do
    name <- Path.fromString "."
    -- putStrLn $ fromJust $ decodeUtf name
    ReadDir.openDirStream name >>= loopDir name
