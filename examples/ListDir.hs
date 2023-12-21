{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Data.Word (Word8)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.OsPath (osp)
import "filepath" System.OsString.Internal.Types (getOsString, getPosixString)
import System.OsString.Data.ByteString.Short (ShortByteString(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
import Streamly.Internal.Data.Array (Array(..))
import GHC.Exts (sizeofByteArray#, Int(..), unsafeCoerce#)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
-- import qualified Streamly.Internal.Data.Unfold as Unfold (either, nil)
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.Handle as Handle

-- | List the current directory recursively using concurrent processing
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    Stream.fold (Handle.writeWith 32000 stdout)
        $ Stream.interposeSuffix 10 Array.reader
        $ fmap (osPathToArray . either id id)

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
        $ Stream.fromPure (Left [osp|.|])
        -- $ Stream.fromPure (Left ".")

    where

    -- unfoldOne = Unfold.either Dir.eitherReaderPaths Unfold.nil
    -- streamOneMaybe = either (Just . Dir.readEitherPaths) (const Nothing)
    streamOne = either Dir.readEitherPaths (const Stream.nil)
    osPathToArray p =
        let !(SBS barr#) = getPosixString $ getOsString p
            mbarr = MutByteArray (unsafeCoerce# barr#)
          in Array mbarr 0 (I# (sizeofByteArray# barr#)) :: Array Word8
