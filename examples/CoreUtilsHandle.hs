-- This is same as the examples in CoreUtils but uses the Handle API instead of
-- the FilePath based APIs.

import Data.Function ((&))
import System.Environment (getArgs)
import System.IO (IOMode(..), stdin, stdout, Handle, openFile)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Array as Array (compactSepByByte_)

-- | Read the contents of a file to stdout.
--
-- Handle.read reads the file in 32KB chunks and converts the chunks into a byte
-- stream. FH.write takes the byte stream as input, converts it into chunks of
-- 32KB and writes those chunks to stdout.
--
catBytes :: Handle -> IO ()
catBytes src =
      Handle.read src     -- Stream IO Word8
    & Stream.fold (Handle.write stdout)   -- IO ()

-- | Chunked version, more efficient than the byte stream version above. Reads
-- the file in 256KB chunks and writes those chunks to stdout.
cat :: Handle -> IO ()
cat src =
      Handle.readChunksWith (256*1024) src    -- Stream IO (Array Word8)
    & Stream.fold (Handle.writeChunks stdout) -- IO ()

-- | Read from standard input write to standard output
echo :: IO ()
echo =
      Handle.readChunks stdin   -- Stream IO (Array Word8)
    & Stream.fold (Handle.writeChunks stdout)  -- IO ()

-- | Copy a source file to a destination file.
--
-- Handle.read reads the file in 32KB chunks and converts the chunks into a byte
-- stream. FH.write takes the byte stream as input, converts it into chunks of
-- 32KB and writes those chunks to the destination file.
cpBytes :: Handle -> Handle -> IO ()
cpBytes src dst =
      Handle.read src  -- Stream IO Word8
    & Stream.fold (Handle.write dst)   -- IO ()

-- | Chunked version, more efficient than the byte stream version above. Reads
-- the file in 256KB chunks and writes those chunks to stdout.
cp :: Handle -> Handle -> IO ()
cp src dst =
      Stream.fold (Handle.writeChunks dst)
    $ Handle.readChunksWith (256*1024) src

-- | Count lines like wc -l.
--
-- Char stream version. Reads the input as a byte stream, splits it into lines
-- and counts the lines..
wclChar :: Handle -> IO Int
wclChar src =
      Handle.read src            -- Stream IO Word8
    & Unicode.decodeLatin1       -- Stream IO Char
    & split (== '\n') Fold.drain -- Stream IO ()
    & Stream.fold Fold.length

    where

    split p f = Stream.foldMany (Fold.takeEndBy_ p f)   -- IO ()

-- | More efficient chunked version. Reads chunks from the input handles and
-- splits the chunks directly instead of converting them into byte stream
-- first.
wcl :: Handle -> IO Int
wcl src =
      Handle.readChunks src      -- Stream IO (Array Word8)
    & Array.compactSepByByte_ 10 -- Stream IO (Array Word8)
    & Stream.fold Fold.length    -- IO ()

main :: IO ()
main = do
    cmd <- fmap head getArgs
    src <- openFile "input.txt" ReadMode
    dst <- openFile "output.txt" WriteMode

    case cmd of
        "cat" -> putStrLn "cat" >> cat src
        "catBytes" -> putStrLn "catBytes" >> catBytes src
        "echo" -> putStrLn "echo" >> echo
        "cp" -> putStrLn "cp" >> cp src dst
        "cpBytes" -> putStrLn "cpBytes" >> cpBytes src dst
        "wcl" -> putStrLn "wcl" >> (wcl src >>= print)
        "wclChar" -> putStrLn "wclChar" >> (wclChar src >>= print)
        _ -> putStrLn $ "Unknown command: " ++ cmd
