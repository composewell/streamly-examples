{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Data.Function ((&))
import Streamly.Internal.FileSystem.Dir (readEitherPaths)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream

-- | List the current directory recursively using concurrent processing
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let start = Stream.fromPure (Left ".")
        mapper = either readEitherPaths (const Stream.nil)
    Stream.parConcatIterate id mapper start
        & Stream.fold (Fold.drainMapM print)
