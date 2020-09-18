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
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Array.Storable.Foreign as A
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as S

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Time.Clock as Clock
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Internal.Network.Socket as SK

------------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------------

sendValue :: Show a => Socket -> a -> IO ()
sendValue sk x =
      S.fromList (show x ++ "\n")
    & U.encodeLatin1
    & S.fold (A.writeN 60)
    >>= SK.writeChunk sk

------------------------------------------------------------------------------
-- Command Handlers
------------------------------------------------------------------------------

time :: Socket -> IO ()
time sk = Clock.getTime Clock.Monotonic >>= sendValue sk

random :: Socket -> IO ()
random sk = (randomIO :: IO Int) >>= sendValue sk

def :: (String, Socket) -> IO ()
def (str, sk) = return ("Unknown command: " ++ str) >>= sendValue sk

commands :: Map.Map String (Fold IO Socket ())
commands = Map.fromList
    [ ("time"  , FL.drainBy time)
    , ("random", FL.drainBy random)
    ]

demux :: Fold IO (String, Socket) ()
demux = FL.demuxDefault_ commands (FL.drainBy def)

------------------------------------------------------------------------------
-- Parse and handle commands on a socket
------------------------------------------------------------------------------

handler :: Socket -> IO ()
handler sk =
      S.unfold SK.read sk             -- SerialT IO Word8
    & U.decodeLatin1                  -- SerialT IO Char
    & U.words FL.toList               -- SerialT IO String
    & S.map (, sk)                    -- SerialT IO (String, Socket)
    & S.fold demux                    -- IO () + Exceptions
    & discard                         -- IO ()

------------------------------------------------------------------------------
-- Accept connecttions and handle connected sockets
------------------------------------------------------------------------------

server :: IO ()
server =
      (S.serially $ S.unfold TCP.acceptOnPort 8091)  -- SerialT IO Socket
    & (S.asyncly  . S.mapM (SK.handleWithM handler)) -- AsyncT IO ()
    & S.drain                                        -- IO ()

main :: IO ()
main = server
