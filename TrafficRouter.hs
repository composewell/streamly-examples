-- Traffic router multiplexes incoming traffic from multiple senders. Senders
-- connect at TCP port 8090 and send messages separated by newlines. Lines from
-- different senders are multiplexed into a single line stream. The unified
-- line stream is sent to all the connected receivers. Receivers connect at
-- port 8091.
--
-- To multiplex messages containing arbitrary data, encode each message using
-- base64 encoding and separate the resulting messages with a newline
-- character.
--
-- Use GHC 8.6.5 to build this example, GHC 8.8.1 has some GC issues:
-- TrafficRouter +RTS -N
--
-- For marginal perf improvements try the following RTS options:
-- TrafficRouter +RTS -N -qn2 -A32m -kc1m
--
-- Senders:
-- cat infile.txt | nc <server> 8090
--
-- Receivers:
-- nc <server> 8091 > /dev/null

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Catch (finally)
import Data.Function ((&))
import Data.IORef
import Data.Word (Word8)
import Network.Socket (Socket, close, SocketOption(..), PortNumber)

import Streamly
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.Unfold (Unfold)
import Streamly.Internal.Control.Monad (discard)
import Streamly.Memory.Array (Array)

import qualified Streamly as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S

import qualified Streamly.Internal.Data.KeyValue as KV
-- import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.KeyValue as KVF
import qualified Streamly.Internal.Data.Unicode.Stream as U
import qualified Streamly.Internal.Network.Socket as SK
import qualified Streamly.Internal.Network.Inet.TCP as TCP

------------------------------------------------------------------------------
-- Accept connections and handle connected sockets
------------------------------------------------------------------------------

acceptOn :: MonadIO m => Unfold m PortNumber Socket
acceptOn = TCP.acceptOnPortWith [(ReuseAddr,1)]

recvHandler
    :: IORef (Fold IO (Array Word8) (KV.KVList Socket (IORef (Fold IO (Array Word8) ()))))
    -> Socket
    -> IO ()
recvHandler _ref sk =
      S.unfold SK.read sk             -- SerialT IO Word8
    & U.decodeLatin1                  -- SerialT IO Char
    & U.words FL.toList               -- SerialT IO String
    & S.map (, sk)                    -- SerialT IO (String, Socket)
    -- & S.fold demux                    -- IO () + Exceptions
    & S.drain                         -- IO () + Exceptions
    & discard                         -- IO ()

-- Ideally, we can use concurrent composable scans/pipes here. Each concurrent
-- pipe would process a single socket and emit events of interest. All these
-- socket streams are then merged into a single stream which will produce the
-- fold that we want to operate on.

updateFolds
    :: MVar ()
    -> IORef (Fold IO (Array Word8) (KV.KVList Socket (IORef (Fold IO (Array Word8) ()))))
    -> IO ()
updateFolds mvar ref =
      (S.serially $ S.unfold acceptOn 8091)  -- SerialT IO Socket
    & (S.asyncly  . S.mapM handleSocket)     -- AsyncT IO ()
    & S.drain                                -- IO ()

    where

    handleSocket sk = do
        -- f <- FL.mkAsync_ (SK.writeChunks sk)
        -- let f = SK.writeChunks sk
        -- f <- FL.mkAsync_ (SK.writeChunksWithBufferOf (4*1024) sk)
        let f = SK.writeChunksWithBufferOf (4*1024) sk
        finally (KVF.addFoldLocked mvar ref (sk, f) >> recvHandler ref sk)
                (KVF.removeFoldLocked mvar ref sk >> discard (close sk))

------------------------------------------------------------------------------
-- Senders
------------------------------------------------------------------------------

{-# INLINE recvPackets #-}
recvPackets :: SerialT IO (Array Word8)
recvPackets = do
      S.unfold acceptOn 8090        -- SerialT IO Socket
    & S.concatMapWith parallel recv  -- SerialT IO (Array Word8)

    where

    recv sk = S.finally (liftIO $ close sk) (frames sk)

    -- Use Base64 encoding on compressed message with newline as separator to
    -- frame the input.  Labels for demultiplexing the message can be included
    -- in the header, the header can be separated by the body using a special
    -- character other than newline.
    -- {-# NOINLINE frames #-}
    frames :: Socket -> SerialT IO (Array Word8)
    frames sk =
          S.unfold SK.read sk
        -- XXX S.splitWithSuffix can be optimized, try with plugin
        -- & S.splitWithSuffix (== 10) (A.writeN 512)
        & S.splitWithSuffix (== 10) A.write

_mainRecvChunksSingle :: IO ()
_mainRecvChunksSingle = do
      S.unfold acceptOn 8090         -- SerialT IO Socket
    & S.concatUnfold SK.readChunks   -- SerialT IO (Array Word8)
    & S.drain

_mainRecvChunksParallel :: IO ()
_mainRecvChunksParallel = do
      S.unfold acceptOn 8090                 -- SerialT IO Socket
    & S.concatMapWith parallel (SK.toChunks) -- SerialT IO (Array Word8)
    & S.drain

_mainRecvReChunkParallel :: IO ()
_mainRecvReChunkParallel = do
      S.unfold acceptOn 8090           -- SerialT IO Socket
    & S.concatMapWith parallel reChunk -- SerialT IO (Array Word8)
    & S.drain

    where

    reChunk sk =
          S.unfold SK.read sk
        & S.splitWithSuffix (== 10) (A.writeN 512)


mainRouter :: IO ()
mainRouter = do
    ref <- newIORef KVF.emptyKVFold
    mvar <- newMVar ()
    _ <- forkIO (updateFolds mvar ref)
    S.drain $ S.mapM (discard . KVF.runFoldRefStep ref) recvPackets

main :: IO ()
-- main = _mainRecvChunksSingle
-- main = _mainRecvChunksParallel
-- main = _mainRecvReChunkParallel
-- main = S.drain $ recvPackets
main = mainRouter
