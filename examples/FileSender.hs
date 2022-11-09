{-# LANGUAGE FlexibleContexts #-}
-- A TCP client that does the following:
--
-- o Reads multiple filenames passed on the command line
-- o Opens as many concurrent connections to the server
-- o Sends all the files concurrently to the server

import Data.Function ((&))
import Network.Socket (Socket)
import System.Environment (getArgs)
import System.IO (Handle, withFile, IOMode(..))

import qualified Control.Monad.Catch as Catch
import qualified Network.Socket as Net
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Data.Stream as Stream

main :: IO ()
main = do
    files <- getArgs
    sendAll files

    where

    connect :: IO Socket
    connect = TCP.connect (127,0,0,1) 8090

    fileToSocket :: Handle -> Socket -> IO ()
    fileToSocket fh sk =
          Stream.unfold Handle.chunkReader fh  -- Stream IO (Array Word8)
        & Stream.fold (Socket.writeChunks sk)  -- IO ()

    sendFile :: String -> IO ()
    sendFile file =
        withFile file ReadMode $ \fh ->
            Catch.bracket connect Net.close (fileToSocket fh)

    sendAll :: [String] -> IO ()
    sendAll files =
          Stream.fromList files           -- Stream IO String
        & Stream.mapM sendFile            -- Stream IO ()
        & Stream.fold Fold.drain          -- IO ()
