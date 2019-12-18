module Main (main) where

import Control.Monad (when)
import Data.Function ((&))
import Data.Word (Word8)
import Network.Socket (PortNumber)

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unicode.Stream as U

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
      S.repeat "time\nrandom\n"                    -- SerialT IO String
    & U.unwords UF.fromList                        -- SerialT IO Char
    & U.encodeLatin1                               -- SerialT IO Word8
    & TCP.transformBytesWith remoteAddr remotePort -- SerialT IO Word8
    & U.decodeLatin1                               -- SerialT IO Char
    & U.lines FL.drain                             -- SerialT IO String
    & S.chunksOf chunkSize FL.drain                -- SerialT IO ()

main :: IO ()
main = do
      S.replicate 4 sender                        -- SerialT IO (SerialT IO ())
    & S.concatMapWith async id                    -- SerialT IO ()
    & S.postscanlM' (counter "rcvd: ") (0 :: Int) -- SerialT IO Int
    & S.drain                                     -- IO ()
