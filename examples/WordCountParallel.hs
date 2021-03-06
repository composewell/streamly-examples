-- To run this program:
--
-- cabal run --flag fusion-plugin WordCountParallel test-data.txt
--
import Data.Char (chr)
import Data.Function ((&))
import Data.Word (Word8)
import GHC.Conc (numCapabilities)
import System.Environment (getArgs)
import Streamly.Data.Array.Foreign (Array)
import WordCount (count, Counts(..), isSpace)

import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.Internal.FileSystem.File as File (readChunks)
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Stream

-- Get the line, word, char counts in one chunk.
countArray :: Array Word8 -> IO Counts
countArray arr =
      Stream.unfold Array.read arr            -- SerialT IO Word8
    & Stream.decodeLatin1                     -- SerialT IO Char
    & Stream.foldl' count (Counts 0 0 0 True) -- IO Counts

-- When combining the counts in two contiguous chunks, we would also need to
-- know whether the first element of the next chunk was a space char or
-- non-space to know whether the same word is continuing to the next chunk or
-- if it is a new word. So add that too, giving (firstCharWasSpace, Counts).
{-# NOINLINE partialCounts #-}
partialCounts :: Array Word8 -> IO (Bool, Counts)
partialCounts arr = do
    let r = Array.getIndex arr 0
    case r of
        Just x -> do
            counts <- countArray arr
            return (isSpace (chr (fromIntegral x)), counts)
        Nothing -> return (False, Counts 0 0 0 True)

-- Combine the counts from two consecutive chunks
addCounts :: (Bool, Counts) -> (Bool, Counts) -> (Bool, Counts)
addCounts (sp1, Counts l1 w1 c1 ws1) (sp2, Counts l2 w2 c2 ws2) =
    let wcount =
            if not ws1 && not sp2 -- no space between two chunks
            then w1 + w2 - 1
            else w1 + w2
     in (sp1, Counts (l1 + l2) wcount (c1 + c2) ws2)

-- Now put it all together, we only need to divide the stream into arrays,
-- apply our counting function to each array and then combine all the counts.
wc :: String -> IO (Bool, Counts)
wc file = do
      Stream.unfold File.readChunks file -- AheadT IO (Array Word8)
    & Stream.mapM partialCounts          -- AheadT IO (Bool, Counts)
    & Stream.maxThreads numCapabilities  -- AheadT IO (Bool, Counts)
    & Stream.fromAhead                   -- SerialT IO (Bool, Counts)
    & Stream.foldl' addCounts (False, Counts 0 0 0 True) -- IO (Bool, Counts)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    name <- fmap head getArgs
    (_, Counts l w c _) <- wc name
    putStrLn $ show l ++ " " ++ show w ++ " " ++ show c ++ " " ++ name
