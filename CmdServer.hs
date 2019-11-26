{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.Function ((&))
import Network.Socket (Socket)
import Streamly.Data.Fold (Fold)
import System.Random (randomIO)

import qualified Data.Map.Strict as Map
import qualified Streamly as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as S

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Time.Clock as Clock
import qualified Streamly.Internal.Data.Unicode.Stream as U
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

mkFold :: (() -> Socket -> IO ()) -> Fold IO Socket ()
mkFold step = FL.mkFoldId step (return ())

------------------------------------------------------------------------------
-- Command Handlers
------------------------------------------------------------------------------

time :: () -> Socket -> IO ()
time () sk = Clock.getTime Clock.Monotonic >>= sendValue sk

random :: () -> Socket -> IO ()
random () sk = (randomIO :: IO Int) >>= sendValue sk

commands :: Map.Map String (Fold IO Socket ())
commands = Map.fromList
    [ ("time"  , mkFold time)
    , ("random", mkFold random)
    ]

------------------------------------------------------------------------------
-- Parse and handle commands on a socket
------------------------------------------------------------------------------

handler :: Socket -> IO ()
handler sk =
      S.unfold SK.read sk             -- SerialT IO Word8
    & U.decodeLatin1                  -- SerialT IO Char
    & U.words FL.toList               -- SerialT IO String
    & S.map (, sk)                    -- SerialT IO (String, Socket)
    & S.fold (FL.demux_ commands)     -- SerialT IO ()

------------------------------------------------------------------------------
-- Accept connecttions and handle connected sockets
------------------------------------------------------------------------------

server :: IO ()
server =
      S.unfold TCP.acceptOnPort 8091  -- SerialT IO Socket
    & S.serially                      -- AsyncT IO ()
    & S.mapM (SK.handleWithM handler) -- AsyncT IO ()
    & S.asyncly                       -- SerialT IO ()
    & S.drain                         -- IO ()

main :: IO ()
main = server
