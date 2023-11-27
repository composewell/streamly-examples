import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Char (isSpace)
import Data.Function ((&))
import Network.Socket (Socket, close)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Unicode.Stream as Unicode

-- Simulate network/db query by adding a delay
fetch :: String -> IO (String, String)
fetch w = threadDelay 1000000 >> return (w,w)

-- Read lines of whitespace separated list of words from a socket, fetch the
-- meanings of each word concurrently and return the meanings separated by
-- newlines, in same order as the words were received. Repeat until the
-- connection is closed.
lookupWords :: Socket -> IO ()
lookupWords sk =
      Socket.read sk                             -- Stream IO Word8
    & Unicode.decodeLatin1                       -- Stream IO Char
    & Stream.parseMany word
    & Stream.catRights                           -- Stream IO String
    & Stream.parMapM (Stream.ordered True) fetch
    & fmap show                                  -- Stream IO String
    & Stream.intersperse "\n"                    -- Stream IO String
    & Unicode.encodeStrings Unicode.encodeLatin1 -- Stream IO (Array Word8)
    & Stream.fold (Socket.writeChunks sk)        -- IO ()

  where

  word = Parser.wordBy isSpace Fold.toList

serve :: Socket -> IO ()
serve sk = finally (lookupWords sk) (close sk)

-- | Run a server on port 8091. Accept and handle connections concurrently. The
-- connection handler is "serve" (i.e. lookupWords).  You can use "telnet" or
-- "nc" as a client to try it out.
main :: IO ()
main =
      TCP.accept 8091                             -- Stream IO Socket
    & Stream.parMapM id (Socket.forSocketM serve) -- Stream IO ()
    & Stream.fold Fold.drain                      -- IO ()
