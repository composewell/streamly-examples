{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Data.Functor.Identity
import Data.Function ((&))
import Data.Word
import Data.Char
import Data.Map.Strict hiding (map, lookup)
import Network.HTTP.Simple
import Network.Socket

import Streamly.Prelude (SerialT)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Fold.Tee (Tee(..))
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Array.Foreign as A
import qualified Streamly.Network.Socket as SK
import qualified Streamly.Network.Inet.TCP as TCP

import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Tee as Tee
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Network.Socket as SK
import qualified Streamly.Internal.Data.Stream.Parallel as Par

-- | Sum a list of Int
sumInt :: Identity Int
sumInt =
      S.unfold UF.fromList [1..10] -- SerialT Identity Int
    & S.fold   FL.sum              -- Identity Int

-- | Sum a list of Int
sumInt1 :: Identity Int
sumInt1 =
      S.fromList [1..10]       -- SerialT Identity Int
    & S.sum                    -- Identity Int

-- | Read from standard input write to standard output
echo :: IO ()
echo =
      FH.getChunks            -- SerialT IO (Array Word8)
    & FH.putChunks            -- IO ()

-- | Read from a file and write to standard output
cat :: IO ()
cat =
      File.toChunks "inFile"    -- SerialT IO (Array Word8)
    & FH.putChunks              -- IO ()

-- | Read from a file and write to another file
cp :: IO ()
cp =
      File.toChunks "inFile"    -- SerialT IO (Array Word8)
    & File.fromChunks "outFile" -- IO ()

-- | Read from a file and write to two files
tee :: IO ((),())
tee =
      File.toBytes "inFile"        -- SerialT IO Word8
    & S.fold (FL.tee (File.write "outFile1") (File.write "outFile2"))
                               -- IO ((),())
bucket :: Int -> (Int, Int)
bucket n = let i = n `mod` 10
           in if i > 9 then (9,n) else (i,n)

-- | Read from a file and generate a histogram of word length
wordHisto :: IO (Map Int Int)
wordHisto =
      File.toBytes "inFile"            -- SerialT IO Word8
    & U.decodeLatin1                   -- SerialT IO Char
    & U.words FL.length                -- SerialT IO Int
    & S.map bucket                     -- SerialT IO (Int, Int)
    & S.trace print
    & S.fold (FL.classify FL.length)   -- IO (Map (Int, Int))

-- Read all files from a directory and write to a single output file.
-- Equivalent to cat dir/* > outFile.
appendAll :: IO ()
appendAll =
      Dir.toFiles "appendAll"        -- SerialT IO String
    & S.unfoldMany File.read         -- SerialT IO Word8
    & File.fromBytes "outFile"       -- IO()

cross :: Monad m => Unfold m Int Int
cross =
      UF.cross src src
    & UF.map mult

    where

    mult :: (Int, Int) -> Int
    mult (x, y) = x * y

    src :: Monad m => Unfold m Int Int
    src = UF.enumerateFromToIntegral 1000

-- | Nested looping/outer product. Multiply an element of the first stream with
-- all elements of the second stream and then add the results. Do this for all
-- elements of the first stream.
crossMultSum :: IO Int
crossMultSum = UF.fold FL.sum cross 1

-- | Nested looping/outer product. For each element of the first stream create
-- a tuple with each element of the second stream.
--
loops :: SerialT IO ()
loops = do
    x <- S.fromList [1,2 :: Int]
    y <- S.fromList [3,4 :: Int]
    S.yieldM $ putStrLn $ show (x, y)

get :: String -> IO String
get s = liftIO (httpNoBody (parseRequest_ s)) >> return s

fetch :: String -> IO (String, String)
fetch w =
    (,) <$> pure w <*> get ( "https://www.google.com/search?q=" ++ w)

wordList :: [String]
wordList = ["cat", "dog", "mouse"]

meanings :: [IO (String, String)]
meanings = map fetch wordList

-- | Fetch words meanings using google search for words in 'wordList'. All
-- searches are performed concurrently.
--
getWords :: IO ()
getWords =
      S.fromListM meanings                  -- SerialT  IO (String, String)
    & S.aheadly
    & S.map show                            -- SerialT IO String
    & S.intercalateSuffix "\n" UF.identity  -- SerialT IO String
    & S.map A.fromList                      -- SerialT IO (Array Word8)
    & FH.putChunks                          -- IO ()

lookupWords :: Socket -> IO ()
lookupWords sk =
      S.unfold SK.read sk                   -- SerialT IO Word8
    & U.decodeLatin1                        -- SerialT IO Char
    & U.words FL.toList                     -- SerialT IO String
    & S.serially                            -- AheadT  IO String
    & S.mapM fetch                          -- AheadT  IO (String, String)
    & S.aheadly                             -- SerialT IO (String, String)
    & S.map show                            -- SerialT IO String
    & S.intercalateSuffix "\n" UF.identity  -- SerialT IO String
    & S.fold (SK.writeStrings U.encodeLatin1 sk) -- IO ()

serve :: Socket -> IO ()
serve sk = finally (lookupWords sk) (close sk)

-- | Run a server on port 8091. The server accepts lines separated by newline
-- characters, each line may contain a number of words. The server returns the
-- meanings of words after performing google searches for meanings, the server
-- performs searches concurrently. You can use "telnet" or "nc" as a client to
-- try it.
--
wordserver :: IO ()
wordserver =
      S.unfold TCP.acceptOnPort 8091 -- SerialT IO Socket
    & S.serially                     -- AsyncT IO ()
    & S.mapM serve                   -- AsyncT IO ()
    & S.asyncly                      -- SerialT IO ()
    & S.drain                        -- IO ()

readWords :: Socket -> SerialT IO String
readWords sk =
    S.unfold SK.read sk -- SerialT IO Word8
  & U.decodeLatin1      -- SerialT IO Char
  & U.words FL.toList   -- SerialT IO String

recv :: Socket -> SerialT IO String
recv sk = S.finally (liftIO $ close sk) (readWords sk)

-- | Starts a server at port 8091 listening for lines with space separated
-- words. Multiple clients can connect to the server and send lines. The server
-- handles all the connections concurrently, merges the lines and writes the
-- merged stream to a file.
--
mergeStreams :: IO ()
mergeStreams =
      S.unfold TCP.acceptOnPort 8091   -- SerialT IO Socket
    & S.concatMapWith S.parallel recv  -- SerialT IO String
    & U.unwords UF.fromList            -- SerialT IO Char
    & U.encodeLatin1                   -- SerialT IO Word8
    & File.fromBytes "outFile"         -- IO ()

-- | Read from standard input and write to a file, on the way tap the stream
-- and write it to two other files.
concurrentFolds :: IO ()
concurrentFolds =
      FH.getBytes                            -- SerialT IO Word8
    & Par.tapAsync (File.fromBytes "outFile1") -- SerialT IO Word8
    & Par.tapAsync (File.fromBytes "outFile2") -- SerialT IO Word8
    & File.fromBytes "outFile"               -- IO ()

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
    -- S.drain loops
    -- getWords
    -- wordserver
    -- mergeStreams
    concurrentFolds
