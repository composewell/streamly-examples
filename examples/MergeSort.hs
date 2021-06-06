{-# LANGUAGE FlexibleContexts    #-}

-- This example generates two random streams sorted in ascending order and
-- merges them in ascending order, concurrently.
--
-- Compile with '-threaded -with-rtsopts "-N"' GHC options to use the
-- parallelism.

import Data.Word (Word16)
import Streamly.Prelude (SerialT)
import System.Random (getStdGen, randoms)

import qualified Data.List as List
import qualified Streamly.Prelude as Stream

getRandomSorted :: IO (SerialT IO Word16)
getRandomSorted = do
    g <- getStdGen
    let ls = take 100000 (randoms g) :: [Word16]
    return $ Stream.fromList (List.sort ls)

main :: IO ()
main = do
    s1 <- getRandomSorted
    s2 <- getRandomSorted
    Stream.last (Stream.mergeAsyncBy compare s1 s2) >>= print
