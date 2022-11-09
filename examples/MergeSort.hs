{-# LANGUAGE FlexibleContexts    #-}

-- This example generates two random streams sorted in ascending order and
-- merges them in ascending order, concurrently.
--
-- Compile with '-threaded -with-rtsopts "-N"' GHC options to use the
-- parallelism.

import Data.Word (Word16)
import Streamly.Data.Stream (Stream)
import System.Random (getStdGen, randoms)

import qualified Data.List as List
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream

getRandomSorted :: IO (Stream IO Word16)
getRandomSorted = do
    g <- getStdGen
    let ls = take 100000 (randoms g) :: [Word16]
    return $ Stream.fromList (List.sort ls)

main :: IO ()
main = do
    s1 <- getRandomSorted
    s2 <- getRandomSorted
    Stream.fold Fold.latest (Stream.mergeBy compare s1 s2) >>= print
