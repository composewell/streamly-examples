module Main (main) where

import Data.Char (ord)
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import System.Environment (getArgs)
import System.IO (withBinaryFile, IOMode(ReadMode), Handle, stdout)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Scanl as Scanl
import qualified Streamly.FileSystem.Handle as Handle

{-
import Data.Char (chr)
import Streamly.Data.Array (Unbox)
import Streamly.Data.Fold (Fold)
import Streamly.Data.MutArray (MutArray)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Control.Monad.Trans.Class (lift)
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Unicode.Stream as Stream
-}

-- NOTE: Longer patterns are slower in streamly because of byte-by-byte stream
-- processing instead of processing arrays directly, Rabin-Karp and no SIMD.
-- Apparently rg uses two-way string match and SIMD. We can process arrays, use
-- two-way and SIMD in streamly as well.

{-
showStream :: Show a => Stream.Stream IO a -> IO ()
showStream x =
    Stream.fold (Handle.writeWith 32000 stdout)
        $ Stream.encodeLatin1
        $ Stream.unfoldEachEndBy '\n' Unfold.fromList
        $ fmap show x
-}

pat :: String -> Array Word8
pat str = Array.fromList $ fmap (fromIntegral . ord) str

-- output redirected to /dev/null
-- rg -c '^' (270 ms)
-- this (500 ms)
printMatchCount :: Array Word8 -> Handle -> IO ()
printMatchCount str inh = do
    r <- Stream.fold Fold.length
        $ Stream.splitEndBySeq str Fold.drain
        $ Handle.read inh
    print r

-- rg -o '^' (570 ms)
-- this (1.2 s)
printLineNum :: Array Word8 -> Handle -> IO ()
printLineNum str inh = do
    Stream.fold (Fold.drainMapM print)
        $ Stream.postscanl Scanl.length
        $ Stream.splitEndBySeq str Fold.drain
        $ Handle.read inh

-- rg -b -o '^' (670 ms)
-- this (1.5 s)
-- XXX Serialization by show is also slow
printOffsets :: Array Word8 -> Handle -> IO ()
printOffsets str inh = do
    -- XXX print is very slow, rg print is slower though
    -- Though somehow this is slightly faster when redirecting output, need to
    -- check why.
    Stream.fold (Fold.drainMapM print)
    -- showStream
        $ Stream.postscanl (Scanl.tee Scanl.length Scanl.sum)
        $ Stream.splitEndBySeq str Fold.length
        $ Handle.read inh

{-
{-# INLINE unsafeAppend #-}
unsafeAppend :: (MonadIO m, Unbox a) =>
    m (MutArray a) -> Fold m a (MutArray a)
unsafeAppend = Fold.foldlM' MutArray.unsafeSnoc

{-# INLINE append #-}
append :: Fold (StateT (MutArray Word8) IO) Word8 (MutArray Word8)
-- append = unsafeAppend get -- this is much faster
append = MutArray.appendWith sizer get

    where

    sizer 0 = 1024
    sizer x = x + 1024

chunkSize :: Int
chunkSize = 32000

{-# INLINE putArray #-}
putArray :: MutArray Word8 -> StateT (MutArray Word8) IO (Maybe (MutArray Word8))
putArray newArr = do
    let len = MutArray.length newArr
    if len >= chunkSize - 1024
    then do
        lift $ Handle.putChunk stdout (Array.unsafeFreeze newArr)
        arr <- lift$ MutArray.emptyOf' chunkSize
        put arr
        return (Just newArr)
    else do
        put newArr
        return Nothing
-}

-- time rg -U '\n' (771 ms)
-- this (3 s)
printSplits :: Array Word8 -> Handle -> IO ()
printSplits str inh = do
    -- XXX Array.create seems to make it inefficient.
    Stream.fold (Handle.writeChunks stdout)
        $ Stream.splitEndBySeq str Array.create
        -- $ Stream.splitEndBySeq str (Array.createOf 1024) -- 1.4 sec
        -- $ Stream.splitEndBySeq str (Array.unsafeCreateOf 1024) -- 1 sec
        $ Handle.read inh

    -- This is a good design pattern example if we want to accumulate to the
    -- same array. Here instead of allocating a separate array for each split
    -- we are accumulating all the splits into the same array. But this is 2x
    -- slower, again the problem is the "append" fold, if we use appendUnsafe
    -- it becomes much faster.
    {-
    Stream.fold (Handle.writeChunks stdout)
        $ fmap Array.unsafeFreeze
        $ Stream.catMaybes
        $ Stream.evalStateT (MutArray.emptyOf' chunkSize) -- Stream IO (MutArray Word8))
        $ Stream.mapM putArray
        $ Stream.splitEndBySeq str append
        $ Stream.liftInner -- Stream (StateT (MutArray Word8) IO) Word8
        $ Handle.read inh
    -}

    {-
    -- Folding to lists and then unfolding is slower (5 s)
    Stream.fold (Handle.writeWith 32000 stdout)
        $ Stream.unfoldEach Unfold.fromList
        $ Stream.splitEndBySeq str Fold.toList
        $ Handle.read inh
    -}

    {-
    -- XXX putStr is very slow (9 s)
    Stream.fold (Fold.drainMapM (putStr . fmap (chr . fromIntegral)))
        $ Stream.splitEndBySeq str Fold.toList
        $ Handle.read inh
    -}

usage :: IO ()
usage = putStrLn "Usage: prog (count|linenum|offsets|splits|long) <file>"

-- Use generate-random-word-lines.sh to generate the sample text to work on.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [cmd, file] ->
            withBinaryFile file ReadMode $ \h ->
                case cmd of
                    "count"   -> printMatchCount (pat "\n") h
                    "linenum" -> printLineNum (pat "\n") h
                    "offsets" -> printOffsets (pat "\n") h
                    "splits"  -> printSplits (pat "\n") h
                    "long"    -> printMatchCount (pat "abcdefghijklmnopqrstuvwxyz") h
                    _         -> usage
        _ -> usage
