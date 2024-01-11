{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-to-file #-}


module Main (main) where

import Data.Maybe (fromJust)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.OsPath (osp)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.FileSystem.Path as Path

-- | List the current directory recursively using concurrent processing
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    -- Stream.fold (Fold.drain)
    -- Stream.fold (Fold.drainMapM print)
    Stream.fold (Handle.writeWith 32000 stdout)
    -- Stream.fold (Handle.writeChunks stdout)
    -- Stream.fold (Array.lPinnedCompactGE 32000 (Handle.writeChunks stdout))
        $ Stream.interposeSuffix 10 Array.reader
        -- $ Array.compactInterposeGE 10 32000
        -- $ Array.pinnedCompactLE 32000
        $ fmap Path.toChunk
        $ Stream.unfoldMany Unfold.fromList
        $ fmap (either id id)
        -- $ fmap Path.toChunk
        -- $ Stream.catLefts

        -- Serial using unfolds (fastest serial)
        -- $ Stream.unfoldIterateDfs unfoldOne
        -- $ Stream.unfoldIterateBfs unfoldOne
        -- $ Stream.unfoldIterateBfsRev unfoldOne

        -- Serial using streams
        -- $ Stream.concatIterateDfs streamOneMaybe
        -- $ Stream.concatIterateBfs streamOneMaybe
        -- $ Stream.concatIterateBfsRev streamOneMaybe

        -- Serial using stream append and interleave
        -- $ Stream.concatIterateWith Stream.append streamOne
        -- $ Stream.mergeIterateWith Stream.interleave streamOne

        -- Concurrent
        -- XXX To reduce concurrency overhead, perform buffering in each worker
        -- and post the buffer or return [Path] and then unfold it.
        $ Stream.parConcatIterate
            -- ( Stream.eager False . Stream.maxBuffer 1000 . Stream.maxThreads 4)
            id
            streamOne
        -- $ Stream.parConcatIterate (Stream.interleaved True) streamOne
        -- $ Stream.parConcatIterate (Stream.ordered True) streamOne
        $ Stream.fromPure (Left [fromJust $ Path.fromString "."])
        -- $ Stream.fromPure (Left ".")
        -- $ Stream.fromPure (Left [osp|.|])

    where

    -- unfoldOne = Unfold.either Dir.eitherReaderPaths Unfold.nil
    -- streamOneMaybe = either (Just . Dir.readEitherPaths) (const Nothing)
    streamOne = either Dir.readEitherChunks (const Stream.nil)
