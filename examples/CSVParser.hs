-- ghc -O2  -fspec-constr-recursive=10 -fmax-worker-args=16
-- A simplistic CSV processing example.

import Data.Char (chr)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Stream (Stream)
import System.Environment (getArgs)
import System.IO (IOMode(..))

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Handle as Handle
import qualified System.IO as IO
import qualified Streamly.Internal.Data.Array.Stream as ArrayStream
import qualified Streamly.Internal.Data.Stream as Stream (foldManyPost)

main :: IO ()
main = do
    inFile <- fmap head getArgs
    src <- IO.openFile inFile ReadMode

    Stream.unfold Handle.chunkReader src         -- Stream IO (Array Word8)
        & ArrayStream.splitOn 10                 -- Stream IO (Array Word8)
        & Stream.fold (Fold.drainMapM parseLine) -- IO ()

    where

    printList = putStr . map (chr . fromIntegral)

    splitOn p f = Stream.foldManyPost (Fold.takeEndBy_ p f)

    parseLine arr =
        (Stream.unfold Array.reader arr :: Stream IO Word8)
            & splitOn (== 44) Fold.toList            -- Stream IO [Word8]
            & Stream.intersperse [32]                -- Stream IO [Word8]
            & Stream.fold (Fold.drainMapM printList) -- IO ()
        >> putStrLn ""
