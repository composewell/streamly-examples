module Main
    ( main
    , sortMergeCombined
    , sortMergeChunks
    )
where

import Control.Monad (void)
import Data.Function ((&))
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream

inputCount = 1000000

input :: [Int]
input = [inputCount,inputCount - 1..1]

chunkSize :: Int
chunkSize = 32*1024

binCount :: Int
binCount = inputCount / chunkSize + 1

-- | Divide a stream in bins, sort the bins and concat them.
main :: IO ()
main = do
    -- Use an array to store the bins, number the bins from 0 to n.
    -- Store Mutable arrays in the bin array.
    -- Then quicksort/mergesort each bin and then concat them.
    return ()
