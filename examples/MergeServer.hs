import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Network.Socket (Socket, close)
import Streamly.Prelude (SerialT)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Unicode

import qualified Streamly.Internal.Unicode.Stream as Unicode (lines, unlines)
import qualified Streamly.Internal.FileSystem.File as File (fromBytes)

-- | Read a line stream from a socket. Note lines are buffered, we could add
-- a limit to the buffering for safety.
readLines :: Socket -> SerialT IO String
readLines sk =
    Stream.unfold Socket.read sk -- SerialT IO Word8
  & Unicode.decodeLatin1         -- SerialT IO Char
  & Unicode.lines Fold.toList    -- SerialT IO String

recv :: Socket -> SerialT IO String
recv sk = Stream.finally (liftIO $ close sk) (readLines sk)

-- | Starts a server at port 8091 listening for lines with space separated
-- words. Multiple clients can connect to the server and send streams of lines.
-- The server handles all the connections concurrently, merges the incoming
-- streams at line boundaries and writes the merged stream to a file.
main :: IO ()
main =
      Stream.unfold TCP.acceptOnPort 8091        -- SerialT IO Socket
    & Stream.concatMapWith Stream.parallel recv  -- SerialT IO String
    & Unicode.unlines Unfold.fromList            -- SerialT IO Char
    & Unicode.encodeLatin1                       -- SerialT IO Word8
    & File.fromBytes "output.txt"                -- IO ()
