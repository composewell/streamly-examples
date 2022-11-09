-- Implement some simple coreutils commands

import Control.Monad (void)
import Data.Function ((&))
import Data.Word (Word8)
import System.Environment (getArgs)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold (Fold)

import qualified Streamly.Console.Stdio as Stdio
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream

import qualified Streamly.Internal.FileSystem.Dir as Dir (readFiles)
import qualified Streamly.Internal.FileSystem.File as File

-- | > cat input.txt
cat :: IO ()
cat =
      File.readChunks "input.txt"     -- Stream IO (Array Word8)
    & Stream.fold Stdio.writeChunks -- IO ()

-- | Read all files from a directory and write to a single output file.
-- > cat dir/* > output.txt
catDirTo :: IO ()
catDirTo =
      Dir.readFiles "dir"                -- Stream IO String
    & Stream.unfoldMany File.chunkReader -- Stream IO (Array Word8)
    & File.fromChunks "output.txt"       -- IO ()

-- | > cp input.txt output.txt
cp :: IO ()
cp =
      File.readChunks "input.txt"    -- Stream IO (Array Word8)
    & File.fromChunks "output.txt"   -- IO ()

-- | > cat input.txt >> output.txt
append :: IO ()
append =
      File.readChunks "input.txt"      -- Stream IO (Array Word8)
    & File.appendChunks "output.txt"   -- IO ()

-- | > cat input.txt | tee output1.txt > output.txt
tap :: IO ()
tap =
      File.readChunks "input.txt"                   -- Stream IO (Array Word8)
    & Stream.tap (File.writeChunks "output1.txt") -- Stream IO (Array Word8)
    & File.fromChunks "output.txt"                -- IO ()
{-
-- | > cat input.txt | tee output1.txt > output.txt
-- output1.txt is processed/written to in a separate thread.
tapAsync :: IO ()
tapAsync =
      Stream.unfold Stdio.readChunks ()   -- Stream IO (Array Word8)
    & Stream.tapAsyncK
          (File.fromChunks "output1.txt") -- Stream IO (Array Word8)
    & File.fromChunks "output.txt"        -- IO ()
-}

-- | > cat input.txt | tee output1.txt > output.txt
tee :: IO ()
tee =
      File.readChunks "input.txt" -- Stream IO (Array Word8)
    & Stream.fold t               -- IO ((),())
    & void                        -- IO ()

    where

    -- Combine two folds into a single fold
    t :: Fold IO (Array Word8) ((),())
    t = Fold.tee
            (File.writeChunks "output1.txt") -- Fold IO (Array Word8) ()
            (File.writeChunks "output.txt")  -- Fold IO (Array Word8) ()
{-
-- | > grep -c "the" input.txt
grepc :: IO ()
grepc = do
    File.toBytes "input.txt"                                -- Stream IO Word8
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
