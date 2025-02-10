{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}

module Main (main) where

import Data.Maybe (fromJust)
import NaiveDirStream (loopDir)

import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.Posix.ReadDir as ReadDir
import qualified Streamly.Internal.Data.Array as Array
import Streamly.Internal.Data.Array (arrContents)

main :: IO ()
main = do
    name <- Path.fromString "."
    {-
    name <- MutArray.fromCString# "."#
    name1 <- MutArray.snoc 0 name
    -- putStrLn $ fromJust $ decodeUtf name
    -- ReadDir.openDirStream name >>= loopDir name
    MutArray.unsafePinnedAsPtr buf2 (\s _ ->
        -- XXX we are copying the array constructor.
        ReadDir.openDirStreamCString (castPtr s) >>= loopDir buf1)
    -}
    let arr = Path.toChunk name
    ReadDir.openDirStream name
        >>= loopDir (arrContents arr)
