{-# LANGUAGE FlexibleContexts #-}

import Streamly.Prelude (IsStream, MonadAsync, SerialT)
import qualified Streamly.Prelude as Stream
import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe

-- | pipes to streamly
fromPipes :: (IsStream t, MonadAsync m) => Pipe.Producer a m () -> t m a
fromPipes = Stream.unfoldrM unconsP
    where
    -- Adapt Pipe.next to return a Maybe instead of Either
    unconsP p = either (const Nothing) Just <$> Pipe.next p

-- | streamly to pipes
toPipes :: Monad m => SerialT m a -> Pipe.Producer a m ()
toPipes = Pipe.unfoldr unconsEither
    where
    -- Adapt Stream.uncons to return an Either instead of Maybe
    unconsEither :: Monad m => SerialT m a -> m (Either () (a, SerialT m a))
    unconsEither s = maybe (Left ()) Right <$> Stream.uncons s

main :: IO ()
main = do
    Stream.toList (fromPipes (Pipe.each ([1..3]::[Int]))) >>= print
    Pipe.toListM (toPipes (Stream.fromFoldable ([1..3]::[Int]))) >>= print
