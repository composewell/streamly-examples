{-# LANGUAGE FlexibleContexts #-}

import Streamly.Data.Stream (Stream)

import qualified Streamly.Data.Stream as Stream
import qualified Streaming
import qualified Streaming.Prelude as Streaming

-- | streaming to streamly
fromStreaming :: Monad m => Streaming.Stream (Streaming.Of a) m () -> Stream m a
fromStreaming = Stream.unfoldrM Streaming.uncons
--
-- | streamly to streaming
toStreaming :: Monad m => Stream m a -> Streaming.Stream (Streaming.Of a) m ()
toStreaming = Streaming.unfoldr unconsEither
    where
    -- Adapt Stream.uncons to return an Either instead of Maybe
    unconsEither :: Monad m => Stream m a -> m (Either () (a, Stream m a))
    unconsEither s = maybe (Left ()) Right <$> Stream.uncons s

main :: IO ()
main = do
    Stream.toList (fromStreaming (Streaming.each ([1..3]::[Int]))) >>= print
    Streaming.toList (toStreaming (Stream.fromList ([1..3]::[Int]))) >>= print
