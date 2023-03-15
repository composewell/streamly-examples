{-# LANGUAGE FlexibleContexts #-}

import Streamly.Data.Stream (Stream)

import qualified Streamly.Data.Stream as Stream
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit

-- | conduit to streamly
fromConduit :: Monad m => Conduit.ConduitT () a m () -> Stream m a
fromConduit = Stream.unfoldrM Conduit.unconsM . Conduit.sealConduitT

-- | streamly to conduit
toConduit :: Monad m => Stream m a -> Conduit.ConduitT i a m ()
toConduit = Conduit.unfoldM Stream.uncons

main :: IO ()
main = do
    Stream.toList (fromConduit (Conduit.sourceList ([1..3]::[Int]))) >>= print
    Conduit.runConduit (toConduit (Stream.fromList ([1..3]::[Int])) Conduit..| Conduit.consume) >>= print
