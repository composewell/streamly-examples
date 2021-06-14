{-# LANGUAGE FlexibleContexts #-}

import Streamly.Prelude (MonadAsync, SerialT)
import qualified Streamly.Prelude as Stream
import qualified Data.Vector.Fusion.Stream.Monadic as Vector

--  | vector to streamly
fromVector :: (Stream.IsStream t, MonadAsync  m) => Vector.Stream m a -> t m a
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
toVector :: Monad m => SerialT m a -> Vector.Stream m a
toVector = Vector.unfoldrM (Stream.uncons . Stream.adapt)

main :: IO ()
main = do
    Stream.toList (fromVector (Vector.fromList ([1..3]::[Int])))   >>= print
    Vector.toList (toVector (Stream.fromFoldable ([1..3]::[Int]))) >>= print
