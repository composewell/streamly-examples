-- The server accepts a stream of command words separated by space characters
-- and responds with outputs of the commands.
--
-- To try this example, use "telnet 127.0.0.1 8091" on a terminal and type one
-- or more space separated commands e.g. "time random time" followed by a
-- newline.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.Catch (catch, SomeException)
import Data.Char (isSpace)
import Data.Function ((&))
import Network.Socket (Socket)
import Streamly.Data.Fold (Fold)
import System.Random (randomIO)

import qualified Data.Map.Strict as Map
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Array.Unboxed as Array
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Unicode

import qualified Streamly.Internal.Data.Fold as Fold (demux)
import qualified Streamly.Internal.Data.Time.Clock as Clock (getTime, Clock(..))

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

def :: String -> Socket -> IO ()
def str sk = sendValue sk ("Unknown command: " ++ str)

commands :: String -> IO (Fold IO Socket ())
commands cmd =
    case cmd of
        "time"    -> return (Fold.drainBy time)
        "random"  -> return (Fold.drainBy random)
        _         -> return (Fold.drainBy (def cmd))

demux :: Fold IO (String, Socket) ()
demux = void (Fold.demux commands :: Fold IO (String, Socket) (Map.Map String ()))

------------------------------------------------------------------------------
-- Parse and handle commands on a socket
------------------------------------------------------------------------------

handler :: Socket -> IO ()
handler sk =
      Stream.unfold Socket.read sk       -- SerialT IO Word8
    & Unicode.decodeLatin1               -- SerialT IO Char
    & Stream.wordsBy isSpace Fold.toList -- SerialT IO String
    & Stream.map (, sk)                  -- SerialT IO (String, Socket)
    & Stream.fold demux                  -- IO () + Exceptions
    & discard                            -- IO ()

    where

    discard action = void action `catch` (\(_ :: SomeException) -> return ())

------------------------------------------------------------------------------
-- Accept connecttions and handle connected sockets
------------------------------------------------------------------------------

server :: IO ()
server =
      Stream.unfold TCP.acceptOnPort 8091      -- SerialT IO Socket
    & Stream.fromSerial                        -- AsyncT IO Socket
    & Stream.mapM (Socket.forSocketM handler)  -- AsyncT IO ()
    & Stream.fromAsync                         -- SerialT IO ()
    & Stream.drain                             -- IO ()

main :: IO ()
main = server
