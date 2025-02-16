module Main (main) where

import BasicDirStream (loopDir0)
import Streamly.Internal.Data.Array (arrContents)

import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.Posix.ReadDir as ReadDir

main :: IO ()
main = do
    name <- Path.fromString "."
    let arr = Path.toChunk name
    buffer <- MutByteArray.new' 1024
    ReadDir.openDirStream name
        >>= loopDir0 buffer (arrContents arr)
