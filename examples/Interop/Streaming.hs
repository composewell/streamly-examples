{-# LANGUAGE FlexibleContexts #-}

import Streamly.Prelude (IsStream, MonadAsync, SerialT)
import qualified Streamly.Prelude as Stream
import qualified Streaming
import qualified Streaming.Prelude as Streaming

-- | streaming to streamly
fromStreaming :: (IsStream t, MonadAsync m) => Streaming.Stream (Streaming.Of a) m () -> t m a
fromStreaming = Stream.unfoldrM Streaming.uncons
--
-- | streamly to streaming
toStreaming :: Monad m => SerialT m a -> Streaming.Stream (Streaming.Of a) m ()
toStreaming = Streaming.unfoldr unconsEither
    where
    -- Adapt Stream.uncons to return an Either instead of Maybe
    unconsEither :: Monad m => SerialT m a -> m (Either () (a, SerialT m a))
    unconsEither s = maybe (Left ()) Right <$> Stream.uncons s

main :: IO ()
main = do
    Stream.toList (fromStreaming (Streaming.each ([1..3]::[Int]))) >>= print
    Streaming.toList (toStreaming (Stream.fromFoldable ([1..3]::[Int]))) >>= print
