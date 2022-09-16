-- ghc -O2  -fspec-constr-recursive=10 -fmax-worker-args=16
-- A simplistic CSV processing example.

import Data.Char (chr)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Prelude (SerialT)
import System.Environment (getArgs)
import System.IO (IOMode(..))

import qualified Streamly.Data.Array.Unboxed as Array
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Prelude as Stream
import qualified System.IO as IO
import qualified Streamly.Internal.Data.Array.Unboxed.Stream as ArrayStream

main :: IO ()
main = do
    inFile <- fmap head getArgs
    src <- IO.openFile inFile ReadMode

    Stream.unfold Handle.readChunks src -- SerialT IO (Array Word8)
        & ArrayStream.splitOn 10        -- SerialT IO (Array Word8)
        & Stream.mapM_ parseLine        -- IO ()

    where

    printList = putStr . map (chr . fromIntegral)
    parseLine arr =
        (Stream.unfold Array.read arr :: SerialT IO Word8)
            & Stream.splitOn (== 44) Fold.toList -- SerialT IO [Word8]
            & Stream.intersperse [32]            -- SerialT IO [Word8]
            & Stream.mapM_ printList             -- IO ()
        >> putStrLn ""
