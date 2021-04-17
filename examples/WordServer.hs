import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Function ((&))
import Network.Socket (Socket, close)

import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Unicode.Stream as Unicode

import qualified Streamly.Internal.Data.Stream.IsStream as Stream (intercalateSuffix)
import qualified Streamly.Internal.Unicode.Stream as Unicode (words)
import qualified Streamly.Internal.Data.Unfold as Unfold (identity)
import qualified Streamly.Internal.Network.Socket as Socket (writeStrings)

-- Simulate network/db query by adding a delay
fetch :: String -> IO (String, String)
fetch w = threadDelay 1000000 >> return (w,w)

-- Read lines of whitespace separated list of words from a socket, fetch the
-- meanings of each word concurrently and return the meanings separated by
-- newlines, in same order as the words were received. Repeat until the
-- connection is closed.
lookupWords :: Socket -> IO ()
lookupWords sk =
      Stream.unfold Socket.read sk               -- SerialT IO Word8
    & Unicode.decodeLatin1                       -- SerialT IO Char
    & Unicode.words Fold.toList                  -- SerialT IO String
    & Stream.fromSerial                          -- AheadT  IO String
    & Stream.mapM fetch                          -- AheadT  IO (String, String)
    & Stream.fromAhead                           -- SerialT IO (String, String)
    & Stream.map show                            -- SerialT IO String
    & Stream.intercalateSuffix "\n" Unfold.identity -- SerialT IO String
    & Stream.fold (Socket.writeStrings Unicode.encodeLatin1 sk) -- IO ()

serve :: Socket -> IO ()
serve sk = finally (lookupWords sk) (close sk)

-- | Run a server on port 8091. Accept and handle connections concurrently. The
-- connection handler is "serve" (i.e. lookupWords).  You can use "telnet" or
-- "nc" as a client to try it out.
main :: IO ()
main =
      Stream.unfold TCP.acceptOnPort 8091 -- SerialT IO Socket
    & Stream.fromSerial                   -- AsyncT IO ()
    & Stream.mapM serve                   -- AsyncT IO ()
    & Stream.fromAsync                    -- SerialT IO ()
    & Stream.drain                        -- IO ()
