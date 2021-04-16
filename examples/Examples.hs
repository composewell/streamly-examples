{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Data.Functor.Identity (Identity)
import Data.Function ((&))
import Data.Map.Strict hiding (map, lookup)
import System.IO (stdin, stdout)
import Network.Socket (Socket, close)

import Streamly.Prelude (SerialT)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Unfold (Unfold)
import Streamly.Internal.Data.Fold.Tee (Tee(..))

import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Prelude as Stream
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Network.Inet.TCP as TCP

import qualified Streamly.Internal.Data.Stream.IsStream as Stream (intercalateSuffix)
import qualified Streamly.Internal.Unicode.Stream as Unicode (words, unwords)
import qualified Streamly.Internal.Data.Unfold as Unfold
    ( fromList, fold, enumerateFromToIntegral, identity, map,
      cross, enumerateFromToIntegral, identity )
import qualified Streamly.Internal.Data.Fold as Fold (classify)
import qualified Streamly.Internal.Data.Fold.Tee as Tee (toFold)
import qualified Streamly.Internal.FileSystem.Dir as Dir (toFiles)
import qualified Streamly.Internal.FileSystem.File as File (fromBytes, toBytes, fromChunks, toChunks, write, read)
import qualified Streamly.Internal.Data.Stream.Parallel as Par (tapAsync)

-- | Sum a list of Int
sumInt :: Identity Int
sumInt =
      Stream.unfold Unfold.fromList [1..10] -- SerialT Identity Int
    & Stream.fold Fold.sum                  -- Identity Int

-- | Sum a list of Int
sumInt1 :: Identity Int
sumInt1 =
      Stream.fromList [1..10]       -- SerialT Identity Int
    & Stream.sum                    -- Identity Int

-- | Read from standard input write to standard output
echo :: IO ()
echo =
      Stream.unfold Handle.readChunks stdin          -- SerialT IO (Array Word8)
    & Stream.fold (Handle.writeChunks stdout)        -- IO ()

-- | Read from a file and write to standard output
cat :: IO ()
cat =
      File.toChunks "inFile"                  -- SerialT IO (Array Word8)
    & Stream.fold (Handle.writeChunks stdout) -- IO ()

-- | Read from a file and write to another file
cp :: IO ()
cp =
      File.toChunks "inFile"    -- SerialT IO (Array Word8)
    & File.fromChunks "outFile" -- IO ()

-- | Read from a file and write to two files
tee :: IO ((),())
tee =
      File.toBytes "inFile"        -- SerialT IO Word8
    & Stream.fold (Fold.tee (File.write "outFile1") (File.write "outFile2"))
                                   -- IO ((),())
bucket :: Int -> (Int, Int)
bucket n = let i = n `mod` 10
           in if i > 9 then (9,n) else (i,n)

-- | Read from a file and generate a histogram of word length
wordHisto :: IO (Map Int Int)
wordHisto =
      File.toBytes "inFile"                   -- SerialT IO Word8
    & Unicode.decodeLatin1                    -- SerialT IO Char
    & Unicode.words Fold.length               -- SerialT IO Int
    & Stream.map bucket                       -- SerialT IO (Int, Int)
    & Stream.trace print
    & Stream.fold (Fold.classify Fold.length) -- IO (Map (Int, Int))

-- Read all files from a directory and write to a single output file.
-- Equivalent to cat dir/* > outFile.
appendAll :: IO ()
appendAll =
      Dir.toFiles "appendAll"        -- SerialT IO String
    & Stream.unfoldMany File.read    -- SerialT IO Word8
    & File.fromBytes "outFile"       -- IO()

cross :: Monad m => Unfold m Int Int
cross =
      Unfold.cross src src
    & fmap mult

    where

    mult :: (Int, Int) -> Int
    mult (x, y) = x * y

    src :: Monad m => Unfold m Int Int
    src = Unfold.enumerateFromToIntegral 1000

-- | Nested looping/outer product. Multiply an element of the first stream with
-- all elements of the second stream and then add the results. Do this for all
-- elements of the first stream.
crossMultSum :: IO Int
crossMultSum = Stream.fold Fold.sum $ Stream.unfold cross 1

-- | Nested looping/outer product. For each element of the first stream create
-- a tuple with each element of the second stream.
--
loops :: SerialT IO ()
loops = do
    x <- Stream.fromList [1,2 :: Int]
    y <- Stream.fromList [3,4 :: Int]
    Stream.yieldM $ putStrLn $ show (x, y)

-- Simulate network/db query by adding a delay
fetch :: String -> IO (String, String)
fetch w = threadDelay 1000000 >> return (w,w)

wordList :: [String]
wordList = ["cat", "dog", "mouse"]

meanings :: [IO (String, String)]
meanings = map fetch wordList

-- | Fetch words meanings for words in 'wordList'. All searches are performed
-- concurrently.
--
getWords :: IO ()
getWords =
      Stream.fromListM meanings                     -- SerialT  IO (String, String)
    & Stream.fromAhead
    & Stream.map show                               -- SerialT IO String
    & Stream.intercalateSuffix "\n" Unfold.identity -- SerialT IO String
    & Stream.map Array.fromList                     -- SerialT IO (Array Word8)
    & Stream.fold (Handle.writeChunks stdout)       -- IO ()

-- | Read from standard input and write to a file, on the way tap the stream
-- and write it to two other files.
concurrentFolds :: IO ()
concurrentFolds =
      Stream.unfold Handle.read stdin          -- SerialT IO Word8
    & Par.tapAsync (File.fromBytes "outFile1") -- SerialT IO Word8
    & Par.tapAsync (File.fromBytes "outFile2") -- SerialT IO Word8
    & File.fromBytes "outFile"                 -- IO ()

main :: IO ()
main = do
    -- print $ runIdentity sumInt
    -- print $ runIdentity sumInt1
    -- echo
    -- cat
    -- cp
    -- void tee
    -- wordHisto >>= print
    -- appendAll
    -- crossMultSum >>= print
    -- Stream.drain loops
    -- getWords
    concurrentFolds
