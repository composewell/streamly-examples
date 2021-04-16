module Main (main) where

import Control.Monad (when)
import Data.Function ((&))
import Data.Word (Word8)
import Network.Socket (PortNumber)

import Streamly.Prelude (SerialT)
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Network.Inet.TCP as TCP (transformBytesWith)
import qualified Streamly.Internal.Unicode.Stream as Unicode (unwords, lines)
import qualified Streamly.Unicode.Stream as Unicode

remoteAddr :: (Word8,Word8,Word8,Word8)
remoteAddr = (192, 168, 1, 4)

remotePort :: PortNumber
remotePort = 8091

chunkSize :: Int
chunkSize = 10000

nChunks :: Int
nChunks = 10

counter :: String -> Int -> () -> IO Int
counter tag n () = do
    let i = n + 1
    when (i `mod` nChunks == 0) $
        putStrLn $ tag ++ show (i * chunkSize)
    return i

sender :: SerialT IO ()
sender =
      Stream.repeat "time\nrandom\n"               -- SerialT IO String
    & Unicode.unwords Unfold.fromList              -- SerialT IO Char
    & Unicode.encodeLatin1                         -- SerialT IO Word8
    & TCP.transformBytesWith remoteAddr remotePort -- SerialT IO Word8
    & Unicode.decodeLatin1                         -- SerialT IO Char
    & Unicode.lines Fold.drain                     -- SerialT IO String
    & Stream.chunksOf chunkSize Fold.drain         -- SerialT IO ()

main :: IO ()
main = do
      Stream.replicate 4 sender                    -- SerialT IO (SerialT IO ())
    & Stream.concatMapWith Stream.async id         -- SerialT IO ()
    & Stream.postscanlM' (counter "rcvd: ")
        (return 0 :: IO Int)                       -- SerialT IO Int
    & Stream.drain                                 -- IO ()
