{-# Language ScopedTypeVariables #-}

-- | Removing the print statements in both C program and Haskell program. C
-- takes 58ms CPU time, Haskell +RTS -s reports 60ms CPU time (time reports
-- 62ms which includes 2ms of RTS startup/teardown overhead). Actual Haskell
-- processing is approximately 3% slower.
--
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
import System.IO
import System.IO.Unsafe

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MutArray
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

loopDir :: Path -> DirStream -> IO ()
loopDir dirname dirp =  goDir

    where

    -- buf is a mutable variable holding dirname/
    -- NOINLINE impacts performance by around 10%.
    -- Goof abstractions for mutable references may be helpful in such
    -- situations.
    {-# NOINLINE buf #-}
    buf :: MutArray Word8
    buf = unsafePerformIO $ do
            -- pinning reduces allocations, avoids copying for system call.
            -- the impact is around 10% cpu time.
            arr <- MutArray.emptyOf' 1024
            arr1 <- MutArray.splice arr (Array.unsafeThaw (Path.toChunk dirname))
            -- putStrLn "unsafeperform"
            MutArray.unsafeSnoc arr1 47

    goDir = do
        resetErrno
        ptr <- c_readdir dirp -- streaming read, no buffering
        if (ptr /= nullPtr)
        then do
            let dname = #{ptr struct dirent, d_name} ptr
            dtype :: #{type unsigned char} <- #{peek struct dirent, d_type} ptr

            -- fullName <- Path.appendCString dirname dname

            if dtype == (#const DT_DIR) -- dir/no-symlink check
            then do
                isMeta <- isMetaDir dname
                when (not isMeta) $ do
                    buf1 <- MutArray.appendCString buf dname

                    -- XXX putStrLn seems to be very inefficient compared to
                    -- the C printf. Adding printf in the C version of this
                    -- program does not make any noticeable difference, whereas
                    -- this makes a huge difference 134ms become 264ms. One big
                    -- reason for that is unicode decoding and the encoding
                    -- again, we should use binary IO without conversion.
                    -- putStrLn $ fromJust $ decodeUtf fullName
                    -- putStrLn $ Path.toString fullName

                    {-
                    arr <- MutArray.emptyOf' 1024
                    arr1 <- MutArray.splice arr buf1
                    arr2 <- MutArray.unsafeSnoc arr1 10

                    MutArray.unsafePinnedAsPtr
                        -- (Path.toChunk fullName)
                        arr2
                        (hPutBuf stdout)
                    -}

                    -- fullName <- Path.appendCString dirname dname
                    let fullName = Path.unsafeFromChunk (Array.unsafeFreeze buf1)
                    buf2 <- MutArray.unsafeSnoc buf1 0
                    MutArray.unsafePinnedAsPtr buf2 (\s _ ->
                        -- XXX we are copying the array constructor.
                        ReadDir.openDirStreamCString (castPtr s) >>= loopDir fullName)
                    {-
                    ReadDir.openDirStream fullName >>= loopDir fullName
                    -}
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
                then ReadDir.closeDirStream dirp
                else throwErrno "loopDir"
