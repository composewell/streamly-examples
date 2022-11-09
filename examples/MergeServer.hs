import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Network.Socket (Socket, close)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import System.IO (Handle, withFile, IOMode(..))

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Unicode.Stream as Unicode

-- | Read a line stream from a socket. Note, lines are buffered, we could add
-- a limit to the buffering for safety.
readLines :: Socket -> Stream IO (Array Char)
readLines sk =
    Stream.unfold Socket.reader sk               -- Stream IO Word8
  & Unicode.decodeLatin1                         -- Stream IO Char
  & split (== '\n') Array.write                  -- Stream IO String

  where

  split p f = Stream.foldMany (Fold.takeEndBy p f)

recv :: Socket -> Stream IO (Array Char)
recv sk = Stream.finallyIO (liftIO $ close sk) (readLines sk)

-- | Starts a server at port 8091 listening for lines with space separated
-- words. Multiple clients can connect to the server and send streams of lines.
-- The server handles all the connections concurrently, merges the incoming
-- streams at line boundaries and writes the merged stream to a file.
server :: Handle -> IO ()
server file =
      Stream.unfold TCP.acceptorOnPort 8090         -- Stream IO Socket
    & Concur.parConcatMap (Concur.eager True) recv -- Stream IO (Array Char)
    & Stream.unfoldMany Array.reader                -- Stream IO Char
    & Unicode.encodeLatin1                          -- Stream IO Word8
    & Stream.fold (Handle.write file)               -- IO ()

main :: IO ()
main = withFile "output.txt" AppendMode server
