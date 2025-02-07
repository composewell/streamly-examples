module Main (main) where

import Control.Monad (when)
import Data.Function ((&))
import Data.Word (Word8)
import Network.Socket (PortNumber)
import Streamly.Data.Stream.Prelude (Stream)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Scanl as Scanl
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Network.Inet.TCP as TCP (pipeBytes)
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

sender :: Stream IO ()
sender =
      Stream.repeat "time\nrandom\n"               -- Stream IO String
    & Stream.unfoldEach Unfold.fromList            -- Stream IO Char
    & Unicode.encodeLatin1                         -- Stream IO Word8
    & TCP.pipeBytes remoteAddr remotePort          -- Stream IO Word8
    & Unicode.decodeLatin1                         -- Stream IO Char
    & split (== '\n') Fold.drain                   -- Stream IO String
    & Stream.foldMany chunk                        -- Stream IO ()

    where

    chunk = Fold.take chunkSize Fold.drain
    split p f = Stream.foldMany (Fold.takeEndBy_ p f)

main :: IO ()
main = do
      Stream.replicate 4 sender                    -- Stream IO (Stream IO ())
    & Stream.parConcat id                          -- Stream IO ()
    & Stream.postscanl counts                      -- Stream IO Int
    & Stream.fold Fold.drain                       -- IO ()

    where

    counts = Scanl.mkScanlM (counter "rcvd") (return 0 :: IO Int)
