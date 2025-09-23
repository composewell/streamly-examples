{-# LANGUAGE TupleSections #-}

import Control.Monad.IO.Class (MonadIO)
import Control.Monad (when)
import Data.Functor.Identity (Identity(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Scanl (Scanl)
import Streamly.Internal.Data.MutArray (MutArray)
-- import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Scanl as Scanl

-- The Genuine Sieve of Eratosthenes
-- https://www.cs.hmc.edu/%7Eoneill/papers/Sieve-JFP.pdf

-- XXX put this in streamk benchmarks
-- 24 minute
naivePrimeStreamK :: StreamK IO Int
naivePrimeStreamK = filterPrime (StreamK.fromStream (Stream.enumerateFrom 2))

    where

    filterPrime ys = StreamK.concatEffect $ do
        Just (p, xs) <- StreamK.uncons ys
        pure
            $ StreamK.cons p
            $ filterPrime
            $ flip StreamK.concatMap xs
            $ \x ->
                if x `mod` p /= 0
                then StreamK.fromPure x
                else StreamK.nil

-- The example listed on the Haskell website divides all numbers by all the
-- previous primes, it is slow take around 170 sec to find primes up to
-- 1,000,000.
naivePrimeList :: [Int]
naivePrimeList = filterPrime [2..]

    where

    filterPrime (p:xs) =
        p : filterPrime [x | x <- xs, x `mod` p /= 0]

-- This has lowest memory residency but highest number of allocations which
-- makes it slow - 8 sec for primes up to 1,000,000. Is it possible to reduce
-- the allocations?
primeStreamK :: StreamK Identity Int
primeStreamK =
    3 `StreamK.cons` StreamK.fromStream (flip Stream.filter (Stream.enumerateFromThen 5 7) $ \n ->
          all (\p -> n `mod` p /= 0)
        $ Stream.takeWhile (\p -> p * p <= n)
        $ StreamK.toStream primeStreamK)

-- 715 ms
{-# INLINE sieveList #-}
sieveList :: Monad m => Stream m Int -> Stream m Int
sieveList =
    Stream.postscanlMaybe $ fmap snd $ (Scanl.mkScanlM (\(primes, _) n -> do
            return $
                let ps = takeWhile (\p -> p * p <= n) primes
                 in if all (\p -> n `mod` p /= 0) ps
                    then (primes ++ [n], Just n)
                    else (primes, Nothing)) (return ([2], Just 2)))

-- 247 ms for primes under a million. This has excellent performance because of
-- list fusion.
primeList :: [Int]
primeList =
    3 : (flip filter [5,7..] $ \n ->
          all (\p -> n `mod` p /= 0)
        $ takeWhile (\p -> p * p <= n) primeList)

-- 240ms for primes up to 1 million
{-# INLINE primesFold #-}
primesFold :: MonadIO m => Fold m Int (MutArray Int)
primesFold =
      flip Fold.foldlM' (pure MutArray.empty)
    $ \arr n -> do
        isPrime <-
            Stream.fold (Fold.all (\p -> n `mod` p /= 0))
                $ Stream.takeWhile (\p -> p * p <= n)
                $ MutArray.read arr
        if isPrime
        then MutArray.snoc arr n
        else pure arr

-- Fastest - 234ms for primes up to 1 million
{-# INLINE primesScan #-}
primesScan :: MonadIO m => Scanl m Int (Maybe Int)
primesScan =
      fmap snd
    $ flip Scanl.mkScanlM (pure (MutArray.empty, Nothing))
    $ \(arr, _) n -> do
        isPrime <-
            Stream.fold (Fold.all (\p -> n `mod` p /= 0))
                $ Stream.takeWhile (\p -> p * p <= n)
                $ MutArray.read arr
        if isPrime
        then fmap (, Just n) (MutArray.snoc arr n)
        else pure (arr, Nothing)

main :: IO ()
main = do
    print $ last $ take 78497 $ naivePrimeList
    {-
    (    Stream.fold Fold.last
        $ Stream.take 100000
        $ StreamK.toStream
        $ naivePrimeStreamK
        ) >>= print
    -}
    {-
    x <- Stream.fold Fold.last $ sieveList $ Stream.enumerateFromThenTo 3 5 1000000
    print x
    -}
    {-
    print
        $ runIdentity
        $ Stream.fold Fold.last
        $ Stream.take 78497
        $ StreamK.toStream
        $ primeStreamK
    -}
    -- print $ last $ take 78497 $ primeList
    {-
    xs <- Stream.fold primesFold $ Stream.enumerateFromThenTo 3 5 1000000
    MutArray.getIndexRev 0 xs >>= print
    -}
    {-
    x <- Stream.fold Fold.last
            $ Stream.postscanlMaybe primesScan
            $ Stream.enumerateFromThenTo 3 5 1000000
    print x
    -}
    return ()
