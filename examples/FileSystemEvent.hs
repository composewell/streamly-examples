{-# LANGUAGE CPP #-}
-- Report all events recursively under the paths provided as arguments
module Main (main) where

import Data.Function ((&))
import System.Environment (getArgs)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.FileSystem.Event as Event
import qualified Streamly.Internal.FS.Event as Event (showEvent)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    paths <- mapM Path.fromString args
    Event.watch (NonEmpty.fromList paths)
        & Stream.fold (Fold.drainMapM (putStrLn . Event.showEvent))
