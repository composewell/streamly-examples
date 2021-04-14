-- A TCP client that does the following:
-- * Reads multiple filenames passed on the command line
-- * Opens as many concurrent connections to the server
-- * Sends all the files concurrently to the server

import System.Environment (getArgs)

import qualified Streamly.Prelude as Stream
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Network.Inet.TCP as TCP (writeChunks)

import System.IO (withFile, IOMode(..))

main :: IO ()
main =
    let sendFile file =
            withFile file ReadMode $ \src ->
                  Stream.fold (TCP.writeChunks (127, 0, 0, 1) 8090)
                $ Stream.unfold FH.readChunks src
     in getArgs >>= Stream.drain . Stream.parallely . Stream.mapM sendFile . Stream.fromList
