{-# Language ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

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
-- * static argument transformation is very important in reducing unnecessary
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
-- we pay the cost of collection as well. Therefore, if we simulate stack
-- like mutable allocations in Haskell, when allocating strictly we have to be
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

module BasicDirStream (loopDir0)

where

import Control.Monad (when)
import Data.Char (ord)
import Foreign (Ptr, Word8, nullPtr, peek, peekByteOff, castPtr, plusPtr)
import Foreign.C
    (resetErrno, Errno(..), getErrno, eINTR, throwErrno, CChar, CSize(..))
import Streamly.Internal.FileSystem.Posix.ReadDir (DirStream)
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
import System.IO (hPutBuf, stdout)

import qualified Streamly.Internal.Data.CString as CString
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
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

foreign import ccall unsafe "string.h strlen" c_strlen_pinned
    :: Ptr a -> IO CSize

{-
-- Temporary mutable buffer
-- XXX In a multithreaded program, we will need a per thread mut var. So we
-- have to pass the mutvar in a function scope instead. It would be nice to
-- have first class mutable variable support in Haskell. We can use the current
-- thread-id as a token to create a variable on heap in the ST monad.
buffer :: MutByteArray
buffer = MutByteArray.emptyMutVar' 1024
-}

-- dirname should not be mutated, we can use an immutable ByteArray type for
-- that.
loopDir0 :: MutByteArray -> MutByteArray -> DirStream -> IO ()
loopDir0 buffer dirname0 dirp0 = loopDir dirname0 dirp0

    where

    -- XXX We can use a poke monad to do this more
    -- ergonomically. We can create a monad for composing
    -- CStrings, and equivalent of snprintf.
    allocBuf dirLen dname = do
        dentLen <- fmap fromIntegral $ c_strlen_pinned dname
        buf <- MutByteArray.new' (dirLen + dentLen + 2)
        return buf

    printName arr off = do
        -- If we use an Array then we can use a slice of the same
        -- buffer for printing and for passing to loopDir.
        MutByteArray.pokeAt off arr (10 :: Word8)
        -- hPutBuf is quite expensive compared to C printf.
        -- There should be an API that supplies the buffer directly to kernel.
        -- And we can build a buffer in the loop here and write it when it is
        -- ready.
        MutByteArray.unsafeAsPtr arr $ \p ->
            hPutBuf stdout p (off+1)

    -- XXX static argument transformations can create too many scoping levels,
    -- can there be a more convenient way to achieve this?
    loopDir dirname dirp = goDir

        where

        appendName dirLen buf dname = do
            -- XXX can use putCString variant instead
            MutByteArray.pokeAt 0 buf (0 :: Word8)
            _ <- CString.splice buf dirname
            MutByteArray.pokeAt dirLen buf (47 :: Word8)
            off <- CString.putCString buf (dirLen + 1) dname
            return off

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
                        dirLen <- CString.length dirname
                        buf <- allocBuf dirLen dname
                        off <- appendName dirLen buf dname

                        -- Print the dir name
                        arr <- MutByteArray.unsafePinnedCloneSlice 0 off buf
                        printName arr off

                        MutByteArray.unsafeAsPtr buf $ \s ->
                            ReadDir.openDirStreamCString (castPtr s) >>= loopDir buf
                else do
                    dirLen <- CString.length dirname
                    -- Using a new buffer every time is more expensive
                    -- buf <- allocBuf dirLen dname
                    -- Use a mutable scratch buffer
                    -- XXX If the size exceeds the scratch buffer then allocate
                    -- a new buffer here.
                    off <- appendName dirLen buffer dname
                    printName buffer off
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
