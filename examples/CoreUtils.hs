{-# LANGUAGE QuasiQuotes #-}

-- Implement some simple coreutils commands

import Control.Monad (void)
import Data.Function ((&))
import Data.Word (Word8)
import System.Environment (getArgs)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold (Fold)
import Streamly.FileSystem.Path (path)

import qualified Streamly.Console.Stdio as Stdio
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream

import qualified Streamly.Internal.FileSystem.DirIO as Dir (readFiles)
import qualified Streamly.Internal.FileSystem.FileIO as File

-- | > cat input.txt
cat :: IO ()
cat =
      File.readChunks [path|input.txt|] -- Stream IO (Array Word8)
    & Stream.fold Stdio.writeChunks     -- IO ()

-- | Read all files from a directory and write to a single output file.
-- > cat dir/* > output.txt
catDirTo :: IO ()
catDirTo =
      Dir.readFiles id [path|dir|]       -- Stream IO Path
    -- NOTE: This is a redundant step that we are forced to do as we've still
    -- not introduced FileIO module that uses Path yet.
    & Stream.unfoldEach File.chunkReader -- Stream IO (Array Word8)
    & File.fromChunks [path|output.txt|] -- IO ()

-- | > cp input.txt output.txt
cp :: IO ()
cp =
      File.readChunks [path|input.txt|]  -- Stream IO (Array Word8)
    & File.fromChunks [path|output.txt|] -- IO ()

-- | > cat input.txt >> output.txt
append :: IO ()
append =
      File.readChunks [path|input.txt|]         -- Stream IO (Array Word8)
    & File.writeAppendChunks [path|output.txt|] -- IO ()

-- | > cat input.txt | tee output1.txt > output.txt
tap :: IO ()
tap =
      File.readChunks [path|input.txt|]                 -- Stream IO (Array Word8)
    & Stream.tap (File.writeChunks [path|output1.txt|]) -- Stream IO (Array Word8)
    & File.fromChunks [path|output.txt|]                -- IO ()
{-
-- | > cat input.txt | tee output1.txt > output.txt
-- output1.txt is processed/written to in a separate thread.
tapAsync :: IO ()
tapAsync =
      Stdio.readChunks ()   -- Stream IO (Array Word8)
    & Stream.tapAsyncK
          (File.fromChunks "output1.txt") -- Stream IO (Array Word8)
    & File.fromChunks [path|output.txt|]  -- IO ()
-}

-- | > cat input.txt | tee output1.txt > output.txt
tee :: IO ()
tee =
      File.readChunks [path|input.txt|] -- Stream IO (Array Word8)
    & Stream.fold t                     -- IO ((),())
    & void                              -- IO ()

    where

    -- Combine two folds into a single fold
    t :: Fold IO (Array Word8) ((),())
    t = Fold.tee
            (File.writeChunks [path|output1.txt|]) -- Fold IO (Array Word8) ()
            (File.writeChunks [path|output.txt|])  -- Fold IO (Array Word8) ()
{-
-- | > grep -c "the" input.txt
grepc :: IO ()
grepc = do
    File.toBytes [path|input.txt|]                          -- Stream IO Word8
        & Stream.splitOnSeq (Array.fromList pat) Fold.drain -- Stream IO ()
        & Stream.length                                     -- IO Int
        >>= print . subtract 1                              -- IO ()

    where

    pat :: [Word8]
    pat = map (fromIntegral . ord) "the"
-}

main :: IO ()
main = do
    cmd <- fmap head getArgs

    case cmd of
        "cat" -> putStrLn "cat" >> cat
        "catDirTo" -> putStrLn "catDirTo" >> catDirTo
        "cp" -> putStrLn "cp" >> cp
        "append" -> putStrLn "append" >> append
        "tap" -> putStrLn "tap" >> tap
       -- "tapAsync" -> putStrLn "tapAsync" >> tapAsync
        "tee" -> putStrLn "tee" >> tee
       -- "grepc" -> putStrLn "grepc" >> grepc
        _ -> putStrLn $ "Unknown command: " ++ cmd
