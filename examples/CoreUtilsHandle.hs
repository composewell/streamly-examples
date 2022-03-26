-- This is same as the examples in CoreUtils but uses the Handle API instead of
-- the FilePath based APIs.

import Data.Function ((&))
import System.Environment (getArgs)
import System.IO (IOMode(..), stdin, stdout, Handle, openFile)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Unicode

import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream (splitOn)

-- | Read the contents of a file to stdout.
--
-- Handle.read reads the file in 32KB chunks and converts the chunks into a byte
-- stream. FH.write takes the byte stream as input, converts it into chunks of
-- 32KB and writes those chunks to stdout.
--
catBytes :: Handle -> IO ()
catBytes src =
      Stream.unfold Handle.read src     -- SerialT IO Word8
    & Stream.fold (Handle.write stdout) -- IO ()

-- | Chunked version, more efficient than the byte stream version above. Reads
-- the file in 256KB chunks and writes those chunks to stdout.
cat :: Handle -> IO ()
cat src =
      Stream.unfold Handle.readChunksWith (256*1024, src) -- SerialT IO (Array Word8)
    & Stream.fold (Handle.writeChunks stdout) -- IO ()

-- | Read from standard input write to standard output
echo :: IO ()
echo =
      Stream.unfold Handle.readChunks stdin   -- SerialT IO (Array Word8)
    & Stream.fold (Handle.writeChunks stdout) -- IO ()

-- | Copy a source file to a destination file.
--
-- Handle.read reads the file in 32KB chunks and converts the chunks into a byte
-- stream. FH.write takes the byte stream as input, converts it into chunks of
-- 32KB and writes those chunks to the destination file.
cpBytes :: Handle -> Handle -> IO ()
cpBytes src dst =
      Stream.unfold Handle.read src  -- SerialT IO Word8
    & Stream.fold (Handle.write dst) -- IO ()

-- | Chunked version, more efficient than the byte stream version above. Reads
-- the file in 256KB chunks and writes those chunks to stdout.
cp :: Handle -> Handle -> IO ()
cp src dst =
      Stream.fold (Handle.writeChunks dst)
    $ Stream.unfold Handle.readChunksWith (256*1024, src)

-- | Count lines like wc -l.
--
-- Char stream version. Reads the input as a byte stream, splits it into lines
-- and counts the lines..
wclChar :: Handle -> IO Int
wclChar src =
      Stream.unfold Handle.read src             -- SerialT IO Word8
    & Unicode.decodeLatin1                      -- SerialT IO Char
    & Stream.splitOnSuffix (== '\n') Fold.drain -- SerialT IO ()
    & Stream.length                             -- IO ()

-- | More efficient chunked version. Reads chunks from the input handles and
-- splits the chunks directly instead of converting them into byte stream
-- first.
wcl :: Handle -> IO Int
wcl src =
      Stream.unfold Handle.readChunks src -- SerialT IO (Array Word8)
    & ArrayStream.splitOn 10              -- SerialT IO (Array Word8)
    & Stream.length                       -- IO ()

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
