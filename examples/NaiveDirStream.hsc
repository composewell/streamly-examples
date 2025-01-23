{-# Language ScopedTypeVariables #-}

module NaiveDirStream (loopDir)

where

import Control.Monad
import System.Directory.OsPath
import System.OsPath.Posix
import System.IO
import Data.Maybe
import System.Posix.Directory.PosixPath
import System.Posix.PosixPath.FilePath (peekFilePath)
import System.Posix.Directory.Internals (CDir, CDirent, DirStream(..))

-- import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord)
import Foreign (Ptr, Word8, nullPtr, peek, peekByteOff, castPtr, plusPtr)
import Foreign.C
    (resetErrno, Errno(..), getErrno, eINTR, throwErrno
    , throwErrnoIfMinus1Retry_, CInt(..), CString, CChar, CSize(..))
import Foreign.C.Error (errnoToIOError)

#include <dirent.h>

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

    -- c_readdir  :: Ptr CDir -> IO (Ptr CDirent)
foreign import ccall unsafe "dirent.h readdir"
    c_readdir  :: DirStream -> IO (Ptr CDirent)

foreign import ccall unsafe "string.h strlen" c_strlen
    :: Ptr CChar -> IO CSize

loopDir :: PosixString -> DirStream -> IO ()
loopDir dirname dirp = do
    resetErrno
    ptr <- c_readdir dirp -- streaming read, no buffering
    if (ptr /= nullPtr)
    then do
        let dname = #{ptr struct dirent, d_name} ptr
        dtype :: #{type unsigned char} <- #{peek struct dirent, d_type} ptr
        name <- peekFilePath (castPtr dname)
        let fullName = dirname </> name

        if dtype == (#const DT_DIR) -- dir/no-symlink check
        then do
            isMeta <- isMetaDir dname
            when (not isMeta) $ do
                putStrLn $ fromJust $ decodeUtf fullName
                openDirStream fullName >>= loopDir fullName
        else putStrLn $ fromJust $ decodeUtf fullName
        loopDir dirname dirp
    else do
        errno <- getErrno
        if (errno == eINTR)
        then loopDir dirname dirp
        else do
            let (Errno n) = errno
            if (n == 0)
            then closeDirStream dirp
            else throwErrno "loopDir"
