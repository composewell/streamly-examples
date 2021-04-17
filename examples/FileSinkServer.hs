{-# LANGUAGE FlexibleContexts #-}

-- A concurrent TCP server that:
--
-- o receives connections from clients
-- o splits the incoming data into lines
-- o lines from concurrent connections are merged into a single srteam
-- o writes the line stream to an output file

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Network.Socket (close)
import System.Environment (getArgs)

import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Unicode

import System.IO (withFile, IOMode(..))

main :: IO ()
main = do
    file <- fmap head getArgs
    withFile file AppendMode server

    where

    server src =
          Stream.unfold TCP.acceptOnPort 8090
        & Stream.concatMapWith Stream.parallel use
        & Stream.unfoldMany Array.read
        & Unicode.encodeLatin1
        & Stream.fold (Handle.write src)

    use sk = Stream.finally (liftIO $ close sk) (recv sk)
    recv =
          Stream.splitWithSuffix (== '\n') Array.write
        . Unicode.decodeLatin1
        . Stream.unfold Socket.read
