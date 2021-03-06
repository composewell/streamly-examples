-- Implement some simple coreutils commands

import Control.Monad (void)
import Data.Char (ord)
import Data.Function ((&))
import Data.Word (Word8)
import System.Environment (getArgs)
import Streamly.Data.Array.Foreign (Array)
import Streamly.Data.Fold (Fold)

import qualified Streamly.Console.Stdio as Stdio
import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream (splitOnSeq)
import qualified Streamly.Internal.Data.Stream.Parallel as Par (tapAsync)
import qualified Streamly.Internal.FileSystem.Dir as Dir (toFiles)
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Prelude as Stream

-- | > cat input.txt
cat :: IO ()
cat =
      File.toChunks "input.txt"     -- SerialT IO (Array Word8)
    & Stream.fold Stdio.writeChunks -- IO ()

-- | Read all files from a directory and write to a single output file.
-- > cat dir/* > output.txt
catDirTo :: IO ()
catDirTo =
      Dir.toFiles "dir"                 -- SerialT IO String
    & Stream.unfoldMany File.readChunks -- SerialT IO (Array Word8)
    & File.fromChunks "output.txt"      -- IO ()

-- | > cp input.txt output.txt
cp :: IO ()
cp =
      File.toChunks "input.txt"    -- SerialT IO (Array Word8)
    & File.fromChunks "output.txt" -- IO ()

-- | > cat input.txt >> output.txt
append :: IO ()
append =
      File.toChunks "input.txt"      -- SerialT IO (Array Word8)
    & File.appendChunks "output.txt" -- IO ()

-- | > cat input.txt | tee output1.txt > output.txt
tap :: IO ()
tap =
      File.toChunks "input.txt"                   -- SerialT IO (Array Word8)
    & Stream.tap (File.writeChunks "output1.txt") -- SerialT IO (Array Word8)
    & File.fromChunks "output.txt"                -- IO ()

-- | > cat input.txt | tee output1.txt > output.txt
-- output1.txt is processed/written to in a separate thread.
tapAsync :: IO ()
tapAsync =
      Stream.unfold Stdio.readChunks ()            -- SerialT IO (Array Word8)
    & Par.tapAsync (File.fromChunks "output1.txt") -- SerialT IO (Array Word8)
    & File.fromChunks "output.txt"                 -- IO ()

-- | > cat input.txt | tee output1.txt > output.txt
tee :: IO ()
tee =
      File.toChunks "input.txt" -- SerialT IO (Array Word8)
    & Stream.fold t             -- IO ((),())
    & void                      -- IO ()

    where

    -- Combine two folds into a single fold
    t :: Fold IO (Array Word8) ((),())
    t = Fold.tee
            (File.writeChunks "output1.txt") -- Fold IO (Array Word8) ()
            (File.writeChunks "output.txt")  -- Fold IO (Array Word8) ()

-- | > grep -c "the" input.txt
grepc :: IO ()
grepc = do
    File.toBytes "input.txt"                                -- SerialT IO Word8
        & Stream.splitOnSeq (Array.fromList pat) Fold.drain -- SerialT IO ()
        & Stream.length                                     -- IO Int
        >>= print . subtract 1                              -- IO ()

    where

    pat :: [Word8]
    pat = map (fromIntegral . ord) "the"

main :: IO ()
main = do
    cmd <- fmap head getArgs

    case cmd of
        "cat" -> putStrLn "cat" >> cat
        "catDirTo" -> putStrLn "catDirTo" >> catDirTo
        "cp" -> putStrLn "cp" >> cp
        "append" -> putStrLn "append" >> append
        "tap" -> putStrLn "tap" >> tap
        "tapAsync" -> putStrLn "tapAsync" >> tapAsync
        "tee" -> putStrLn "tee" >> tee
        "grepc" -> putStrLn "grepc" >> grepc
        _ -> putStrLn $ "Unknown command: " ++ cmd
