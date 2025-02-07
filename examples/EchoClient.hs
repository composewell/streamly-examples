module Main (main) where

import Data.Function ((&))
import Data.Word (Word8)
import Network.Socket (PortNumber)
import Streamly.Data.Stream.Prelude (Stream)

import qualified Streamly.Console.Stdio as Stdio
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Unicode.Stream as Unicode


remoteAddr :: (Word8,Word8,Word8,Word8)
remoteAddr = (127, 0, 0, 1)

remotePort :: PortNumber
remotePort = 8091

echo :: Stream IO ()
echo =
      Stream.unfold Stdio.reader ()                -- Stream IO Word8
    & split (== 10) Array.create                   -- Stream IO (Array Word8)
    & TCP.pipeChunks remoteAddr remotePort         -- Stream IO (Array Word8)
    & Stream.unfoldEach Array.reader               -- Stream IO Word8
    & Unicode.decodeLatin1                         -- Stream IO Char
    & Stream.mapM putChar                          -- Stream IO ()

    where

    split p f = Stream.foldMany (Fold.takeEndBy p f)

main :: IO ()
main = Stream.fold Fold.drain echo
