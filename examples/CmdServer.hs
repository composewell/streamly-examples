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
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Stream (parMapM)
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Unicode.Stream as Unicode

import qualified Streamly.Internal.Data.Fold.Extra as Fold (demux)
import qualified Streamly.Internal.Data.Stream as Stream (catRights)
import qualified Streamly.Internal.Data.Time.Clock as Clock
    (getTime, Clock(..))

------------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------------

sendValue :: Show a => Socket -> a -> IO ()
sendValue sk x =
      Stream.fromList (show x ++ "\n")
    & Unicode.encodeLatin1
    & Stream.fold (Array.writeN 60)
    >>= Socket.putChunk sk

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
        "time"    -> return (Fold.drainMapM time)
        "random"  -> return (Fold.drainMapM random)
        _         -> return (Fold.drainMapM (def cmd))

demux :: Fold IO (String, Socket) ()
demux = void (Fold.demux commands :: Fold IO (String, Socket) (Map.Map String ()))

------------------------------------------------------------------------------
-- Parse and handle commands on a socket
------------------------------------------------------------------------------

handler :: Socket -> IO ()
handler sk =
      Stream.unfold Socket.reader sk     -- Stream IO Word8
    & Unicode.decodeLatin1               -- Stream IO Char
    & Stream.parseMany word              -- Stream IO String
    & Stream.catRights
    & fmap (, sk)                        -- Stream IO (String, Socket)
    & Stream.fold demux                  -- IO () + Exceptions
    & discard                            -- IO ()

    where

    word = Parser.wordBy isSpace Fold.toList
    discard action = void action `catch` (\(_ :: SomeException) -> return ())

------------------------------------------------------------------------------
-- Accept connecttions and handle connected sockets
------------------------------------------------------------------------------

server :: IO ()
server =
      Stream.unfold TCP.acceptorOnPort 8091          -- Stream IO Socket
    & Stream.parMapM id (Socket.forSocketM handler)  -- Stream IO ()
    & Stream.fold Fold.drain                         -- IO ()

main :: IO ()
main = server
