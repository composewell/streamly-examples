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
import Data.Map.Strict (Map)
import Network.Socket (Socket)
import Streamly.Data.Fold (Fold)
import System.Random (randomIO)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Unicode.Stream as Unicode

import qualified Streamly.Internal.Data.Time.Clock as Clock
    (getTime, Clock(..))

------------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------------

sendValue :: Show a => Socket -> a -> IO ()
sendValue sk x =
      Stream.fromList (show x ++ "\n")
    & Unicode.encodeLatin1
    & Stream.fold (Array.createOf 60)
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

{-# INLINE demuxKvToMap #-}
demuxKvToMap :: (Monad m, Ord k) =>
    (k -> m (Fold m a b)) -> Fold m (k, a) (Map k b)
demuxKvToMap f = Fold.demuxerToMap fst (fmap (Just . Fold.lmap snd) . f)

demux :: Fold IO (String, Socket) ()
demux = void (demuxKvToMap commands)

------------------------------------------------------------------------------
-- Parse and handle commands on a socket
------------------------------------------------------------------------------

handler :: Socket -> IO ()
handler sk =
      Socket.read sk        -- Stream IO Word8
    & Unicode.decodeLatin1  -- Stream IO Char
    & Stream.parseMany word -- Stream IO String
    & Stream.catRights
    & fmap (, sk)           -- Stream IO (String, Socket)
    & Stream.fold demux     -- IO () + Exceptions
    & discard               -- IO ()

    where

    word = Parser.wordBy isSpace Fold.toList
    discard action = void action `catch` (\(_ :: SomeException) -> return ())

------------------------------------------------------------------------------
-- Accept connecttions and handle connected sockets
------------------------------------------------------------------------------

server :: IO ()
server =
      TCP.accept 8091                                -- Stream IO Socket
    & Stream.parMapM id (Socket.forSocketM handler)  -- Stream IO ()
    & Stream.fold Fold.drain                         -- IO ()

main :: IO ()
main = server
