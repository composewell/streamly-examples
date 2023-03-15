{-# LANGUAGE FlexibleContexts #-}

import Streamly.Data.Stream (Stream)

import qualified Streamly.Data.Stream as Stream
import qualified Data.Vector.Fusion.Stream.Monadic as Vector

--  | vector to streamly
fromVector :: Monad m => Vector.Stream m a -> Stream m a
fromVector = Stream.unfoldrM unconsV
    where
    unconsV v = do
        r <- Vector.null v
        if r
        then return Nothing
        else do
            h <- Vector.head v
            return $ Just (h, Vector.tail v)

--  | streamly to vector
toVector :: Monad m => Stream m a -> Vector.Stream m a
toVector = Vector.unfoldrM Stream.uncons

main :: IO ()
main = do
    Stream.toList (fromVector (Vector.fromList ([1..3]::[Int])))   >>= print
    Vector.toList (toVector (Stream.fromList ([1..3]::[Int]))) >>= print
