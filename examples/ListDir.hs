{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Data.Function ((&))
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream.Concurrent as Concur (ahead2)
import qualified Streamly.Internal.Data.Stream as Stream (concatIterateWith)
import qualified Streamly.Internal.FileSystem.Dir as Dir (readEitherPaths)

-- | List the current directory recursively using concurrent processing
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let start = Stream.fromPure (Left ".")
        mapper = either Dir.readEitherPaths (const Stream.nil)
    Stream.concatIterateWith Concur.ahead2 mapper start
        & Stream.fold (Fold.drainBy print)
