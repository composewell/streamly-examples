{-# LANGUAGE FlexibleContexts #-}

import Streamly.Prelude (IsStream, MonadAsync, SerialT)
import qualified Streamly.Prelude as Stream
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit

-- | conduit to streamly
fromConduit :: (IsStream t, MonadAsync m) => Conduit.ConduitT () a m () -> t m a
fromConduit = Stream.unfoldrM Conduit.unconsM . Conduit.sealConduitT

-- | streamly to conduit
toConduit :: Monad m => SerialT m a -> Conduit.ConduitT i a m ()
toConduit = Conduit.unfoldM Stream.uncons

main :: IO ()
main = do
    Stream.toList (fromConduit (Conduit.sourceList ([1..3]::[Int]))) >>= print
    Conduit.runConduit (toConduit (Stream.fromFoldable ([1..3]::[Int])) Conduit..| Conduit.consume) >>= print
