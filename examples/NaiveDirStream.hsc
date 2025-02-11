{-# Language ScopedTypeVariables #-}
-- {-# Language MagicHash #-}
-- {-# LANGUAGE UnliftedFFITypes #-}
{-# Language BangPatterns #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

-- | Removing the print statements in both C program and Haskell program. C
-- takes 58ms CPU time, Haskell +RTS -s reports 60ms CPU time (time reports
-- 62ms which includes 2ms of RTS startup/teardown overhead). Actual Haskell
-- processing is approximately 3% slower.
--
-- Learnings and notes:
--
-- * Be frugal with allocations, our perf tool can help in precisely
-- pin-pointing the places where more allocations are happening.
--
-- * static argument transformation is very in reducing unnecessary
-- overhead in recursive loops, it becomes especially important when other
-- inefficiencies are removed. It will be nice if compiler can automatically
-- detect and point out such opportunities.
--
-- * Using mutable memory for in a local loop is beneficial due to cache
-- benefits and reduction in allocations/collections. Haskell lacks good
-- mutable memory abstractions which should be fixed.
--
-- * In C we use local storage on stack and the stack unwinds as the function
-- returns, allocations and deallocations are cheaper. Even if we allocate a
-- variable earlier than it is used, we do not pay much cost. In Haskell, there
-- is no such thing as stack, all allocations happen on heap, once we allocate
-- we pay the cost of collection as well. Therefore, if we simulate the stack
-- like mutable allocations in Haskell, when allocating unlazily we have to be
-- careful to allocate only when needed.
--
-- * In multithreaded programs however Haskell might work better because in
-- that case each C thread requires allocation of a stack, in Haskell it is
-- natural as it always uses the heap. Thus the memory requirement of Haskell
-- is lower and there is no limit for stack.
--
-- * XXX there should be a way to automatically suggest NOINLINE when using
-- unsafeperformIO.
--
-- * XXX A CString module to manipulate CStrings conveniently?
--
-- * XXX An efficient unsafe variable length mutable cell/array module?

module NaiveDirStream (loopDir)

where

import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord)
import Data.Maybe
import Foreign (Ptr, Word8, nullPtr, peek, peekByteOff, castPtr, plusPtr)
import Foreign.C
    (resetErrno, Errno(..), getErrno, eINTR, throwErrno
    , throwErrnoIfMinus1Retry_, CInt(..), CString, CChar, CSize(..))
import Foreign.C.Error (errnoToIOError)
import Streamly.Internal.FileSystem.Path (Path)
import Streamly.Internal.FileSystem.Posix.ReadDir (DirStream)
import Streamly.Internal.Data.MutArray (MutArray)
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
import System.IO
import System.IO.Unsafe

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.CString as CString
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.Posix.ReadDir as ReadDir

#include <dirent.h>

{-# INLINE isMetaDir #-}
isMetaDir :: Ptr CChar -> IO Bool
isMetaDir dname = do
    -- XXX Assuming an encoding that maps "." to ".", this is true for
    -- UTF8.
    -- Load as soon as possible to optimize memory accesses
    c1 <- peek dname
    c2 :: Word8 <- peekByteOff dname 1
    if (c1 /= fromIntegral (ord '.'))
    then return False
    else do
        if (c2 == 0)
        then return True
        else do
            if (c2 /= fromIntegral (ord '.'))
            then return False
            else do
                c3 :: Word8 <- peekByteOff dname 2
                if (c3 == 0)
                then return True
                else return False

foreign import ccall unsafe "dirent.h readdir"
    c_readdir  :: DirStream -> IO (Ptr a)

{-
foreign import ccall unsafe "string.h strlen" c_strlen_pinned
    :: MutableByteArray# -> IO CSize
-}

-- dirname should not be mutated, we can use an immutable ByteArray type for
-- that.
loopDir :: MutByteArray -> DirStream -> IO ()
loopDir dirname dirp = goDir

    where

    -- A lazily allocated mutable variable holding dirname/
    -- Lazy allocation does not allocate it when not needed.
    -- Pinning reduces allocations by avoiding copying for system call.
    -- the impact is around 10% cpu time.
    buf :: (MutByteArray, Int)
    buf = MutByteArray.mutVar' 1024 $ \arr -> do
            -- XXX can use putCString variant instead
            MutByteArray.pokeAt 0 arr (0 :: Word8)
            n <- CString.splice arr dirname
            MutByteArray.pokeAt n arr (47 :: Word8)
            return (n+1)

    {-
    {-# NOINLINE dirlen #-}
    dirlen :: Int
    dirlen = unsafePerformIO $ do
        n <- fmap fromIntegral $ CString.length dirname
        return (n+1)
    -}

    goDir = do
        resetErrno
        ptr <- c_readdir dirp -- streaming read, no buffering
        if (ptr /= nullPtr)
        then do
            let dname = #{ptr struct dirent, d_name} ptr
            dtype :: #{type unsigned char} <- #{peek struct dirent, d_type} ptr

            if dtype == (#const DT_DIR) -- dir/no-symlink check
            then do
                isMeta <- isMetaDir dname
                when (not isMeta) $ do
                    let buf1 = fst buf
                        -- getting dirlen from the tuple forces the allocation
                        -- earlier than needed, therefore increases the
                        -- allocations.

                        -- dirlen = snd buf

                    -- Computing it everytime turns out to be less expensive
                    -- than computing it once using unsafePerformIO, because of
                    -- decreased allocs.
                    dirlen <- fmap (+1) $ CString.length dirname
                    off <- CString.putCString buf1 (dirlen) dname

                    -- XXX putStrLn seems to be very inefficient compared to
                    -- the C printf. Adding printf in the C version of this
                    -- program does not make any noticeable difference, whereas
                    -- this makes a huge difference 134ms become 264ms. One big
                    -- reason for that is unicode decoding and the encoding
                    -- again, we should use binary IO without conversion.
                    -- putStrLn $ fromJust $ decodeUtf fullName
                    -- putStrLn $ Path.toString fullName

                    {-
                    arr <- MutByteArray.unsafePinnedCloneSlice 0 off buf
                    MutByteArray.pokeAt off arr (10 :: Word8)
                    MutByteArray.unsafeAsPtr arr $ \p ->
                        hPutBuf stdout p (off+1)
                    -}

                    MutByteArray.unsafeAsPtr buf1 $ \s ->
                        ReadDir.openDirStreamCString (castPtr s) >>= loopDir buf1
            else -- putStrLn $ fromJust $ decodeUtf fullName
                do
                    -- hPutBuf stdout $ Path.toChunk fullName
                    -- putStrLn $ Path.toString fullName
                    -- Array.unsafePinnedAsPtr
                        -- (Path.toChunk fullName)
                        -- (hPutBuf stdout)
                    return ()
            goDir
        else do
            errno <- getErrno
            if (errno == eINTR)
            then goDir
            else do
                let (Errno n) = errno
                if (n == 0)
                then do
                    ReadDir.closeDirStream dirp
                else throwErrno "loopDir"
