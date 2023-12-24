{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
-- import qualified Streamly.Internal.Data.Unfold as Unfold (either, nil)
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.FileSystem.Path as Path

-- | List the current directory recursively using concurrent processing
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    Stream.fold (Handle.writeWith 32000 stdout)
        $ Stream.interposeSuffix 10 Array.reader
        $ fmap (Path.toByteArray . either id id)

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
        $ Stream.parConcatIterate id streamOne
        -- $ Stream.parConcatIterate (Stream.interleaved True) streamOne
        -- $ Stream.parConcatIterate (Stream.ordered True) streamOne
        $ Stream.fromPure (Left $ Path.fromString ".")
        -- $ Stream.fromPure (Left ".")

    where

    -- unfoldOne = Unfold.either Dir.eitherReaderPaths Unfold.nil
    -- streamOneMaybe = either (Just . Dir.readEitherPaths) (const Nothing)
    streamOne = either Dir.readEitherPaths (const Stream.nil)
