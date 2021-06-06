import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Network.Socket (Socket, close)
import Streamly.Data.Array.Foreign (Array)
import Streamly.Prelude (SerialT)
import System.IO (Handle, withFile, IOMode(..))

import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Unicode

-- | Read a line stream from a socket. Note, lines are buffered, we could add
-- a limit to the buffering for safety.
readLines :: Socket -> SerialT IO (Array Char)
readLines sk =
    Stream.unfold Socket.read sk                 -- SerialT IO Word8
  & Unicode.decodeLatin1                         -- SerialT IO Char
  & Stream.splitWithSuffix (== '\n') Array.write -- SerialT IO String

recv :: Socket -> SerialT IO (Array Char)
recv sk = Stream.finally (liftIO $ close sk) (readLines sk)

-- | Starts a server at port 8091 listening for lines with space separated
-- words. Multiple clients can connect to the server and send streams of lines.
-- The server handles all the connections concurrently, merges the incoming
-- streams at line boundaries and writes the merged stream to a file.
server :: Handle -> IO ()
server file =
      Stream.unfold TCP.acceptOnPort 8090        -- SerialT IO Socket
    & Stream.concatMapWith Stream.parallel recv  -- SerialT IO (Array Char)
    & Stream.unfoldMany Array.read               -- SerialT IO Char
    & Unicode.encodeLatin1                       -- SerialT IO Word8
    & Stream.fold (Handle.write file)            -- IO ()

main :: IO ()
main = withFile "output.txt" AppendMode server
