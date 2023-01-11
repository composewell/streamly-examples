{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
-- import qualified Streamly.Internal.Data.Stream as Stream
-- import qualified Streamly.Internal.Data.Unfold as Unfold (either, nil)
import qualified Streamly.Internal.FileSystem.Dir as Dir

-- | List the current directory recursively using concurrent processing
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    Stream.fold (Fold.drainMapM print)
        -- $ Stream.unfoldIterateDfs unfoldOne
        -- $ Stream.unfoldIterateBfs unfoldOne
        -- $ Stream.unfoldIterateBfsRev unfoldOne
        -- $ Stream.concatIterateDfs streamOneMaybe
        -- $ Stream.concatIterateBfs streamOneMaybe
        -- $ Stream.concatIterateBfsRev streamOneMaybe
        -- $ Stream.concatIterateWith Stream.append streamOne
        -- $ Stream.mergeIterateWith Stream.interleave streamOne
        $ Stream.parConcatIterate id streamOne
        -- $ Stream.parConcatIterate (Stream.interleaved True) streamOne
        -- $ Stream.parConcatIterate (Stream.ordered True) streamOne
        $ Stream.fromPure (Left ".")

    where

    -- unfoldOne = Unfold.either Dir.eitherReaderPaths Unfold.nil
    -- streamOneMaybe = either (Just . Dir.readEitherPaths) (const Nothing)
    streamOne = either Dir.readEitherPaths (const Stream.nil)
