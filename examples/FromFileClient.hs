{-# LANGUAGE FlexibleContexts #-}
-- A TCP client that does the following:
--
-- o Reads multiple filenames passed on the command line
-- o Opens as many concurrent connections to the server
-- o Sends all the files concurrently to the server

import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array.Foreign (Array)
import Streamly.Data.Fold (Fold)
import System.Environment (getArgs)
import System.IO (Handle, withFile, IOMode(..))

import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.Network.Inet.TCP as TCP (writeChunks)
import qualified Streamly.Prelude as Stream

main :: IO ()
main = do
    files <- getArgs
    sendAll files

    where

    writeToHost :: Fold IO (Array Word8) ()
    writeToHost = TCP.writeChunks (127, 0, 0, 1) 8090

    sendHandle :: Handle -> IO ()
    sendHandle fh =
          Stream.unfold Handle.readChunks fh  -- SerialT IO (Array Word8)
        & Stream.fold writeToHost             -- IO ()

    sendFile :: String -> IO ()
    sendFile file = withFile file ReadMode sendHandle

    sendAll :: [String] -> IO ()
    sendAll files =
          Stream.fromList files -- ParallelT IO String
        & Stream.mapM sendFile  -- ParallelT IO ()
        & Stream.fromParallel   -- SerialT IO ()
        & Stream.drain          -- IO ()
