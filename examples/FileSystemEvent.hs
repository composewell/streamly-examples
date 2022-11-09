{-# LANGUAGE CPP #-}
-- Report all events recursively under the paths provided as arguments
module Main (main) where

import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Data.Word (Word8)
import System.Environment (getArgs)
import Streamly.Data.Array (Array)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Unicode

#if darwin_HOST_OS
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif linux_HOST_OS
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif mingw32_HOST_OS
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
#error "FS Events not supported on this platform"
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 s = Stream.fold Array.write (Unicode.encodeUtf8' $ Stream.fromList s)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    paths <- mapM toUtf8 args
    Event.watch (NonEmpty.fromList paths)
        & Stream.fold (Fold.drainMapM (putStrLn . Event.showEvent))
