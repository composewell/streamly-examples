-- The server accepts a stream of command words separated by space characters
-- and responds with outputs of the commands.
--
-- To try this example, use "telnet 127.0.0.1 8091" on a terminal and type one
-- or more space separated commands e.g. "time random time" followed by a
-- newline.

{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.Function ((&))
import Network.Socket (Socket)
import Streamly.Data.Fold (Fold)
import System.Random (randomIO)
import Streamly.Internal.Control.Monad (discard)

import qualified Data.Map.Strict as Map
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Unicode

import qualified Streamly.Internal.Data.Fold as Fold (demuxDefault)
import qualified Streamly.Internal.Data.Time.Clock as Clock (getTime, Clock(..))
import qualified Streamly.Internal.Unicode.Stream as Unicode (words)
import qualified Streamly.Internal.Network.Socket as Socket (handleWithM, writeChunk)

------------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------------

sendValue :: Show a => Socket -> a -> IO ()
sendValue sk x =
      Stream.fromList (show x ++ "\n")
    & Unicode.encodeLatin1
    & Stream.fold (Array.writeN 60)
    >>= Socket.writeChunk sk

------------------------------------------------------------------------------
-- Command Handlers
------------------------------------------------------------------------------

time :: Socket -> IO ()
time sk = Clock.getTime Clock.Monotonic >>= sendValue sk

random :: Socket -> IO ()
random sk = (randomIO :: IO Int) >>= sendValue sk

def :: (String, Socket) -> IO ()
def (str, sk) = sendValue sk ("Unknown command: " ++ str)

commands :: Map.Map String (Fold IO Socket ())
commands = Map.fromList
    [ ("time"  , Fold.drainBy time)
    , ("random", Fold.drainBy random)
    ]

demux :: Fold IO (String, Socket) ()
demux = snd <$> Fold.demuxDefault commands (Fold.drainBy def)

------------------------------------------------------------------------------
-- Parse and handle commands on a socket
------------------------------------------------------------------------------

handler :: Socket -> IO ()
handler sk =
      Stream.unfold Socket.read sk    -- SerialT IO Word8
    & Unicode.decodeLatin1            -- SerialT IO Char
    & Unicode.words Fold.toList       -- SerialT IO String
    & Stream.map (, sk)               -- SerialT IO (String, Socket)
    & Stream.fold demux               -- IO () + Exceptions
    & discard                         -- IO ()

------------------------------------------------------------------------------
-- Accept connecttions and handle connected sockets
------------------------------------------------------------------------------

server :: IO ()
server =
      Stream.unfold TCP.acceptOnPort 8091      -- SerialT IO Socket
    & Stream.fromSerial                        -- AsyncT IO Socket
    & Stream.mapM (Socket.handleWithM handler) -- AsyncT IO ()
    & Stream.fromAsync                         -- SerialT IO ()
    & Stream.drain                             -- IO ()

main :: IO ()
main = server
