-- To run this program:
--
-- cabal run --flag fusion-plugin WordCountParallel test-data.txt
--
import Data.Char (chr)
import Data.Function ((&))
import Data.Word (Word8)
import GHC.Conc (numCapabilities)
import System.Environment (getArgs)
import Streamly.Internal.Data.Array.Foreign (Array)
import WordCount (count, Counts(..), isSpace)

import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Stream

{-# NOINLINE countArray #-}
countArray :: Array Word8 -> IO (Bool, Counts)
countArray arr = do
    let r = Array.readIndex arr 0
    case r of
        Just x -> do
            counts <-
                  Stream.unfold Array.read arr
                & Stream.decodeLatin1
                & Stream.foldl' count (Counts 0 0 0 True)
            return (isSpace (chr (fromIntegral x)), counts)
        Nothing -> return (False, Counts 0 0 0 True)

addCounts :: (Bool, Counts) -> (Bool, Counts) -> (Bool, Counts)
addCounts (sp1, (Counts l1 w1 c1 ws1)) (sp2, (Counts l2 w2 c2 ws2)) =
    let wcount = if not ws1 && not sp2 then w1 + w2 - 1 else w1 + w2
     in (sp1, Counts (l1 + l2) wcount (c1 + c2) ws2)

wc :: String -> IO (Bool, Counts)
wc file = do
      Stream.unfold File.readChunks file
    & Stream.mapM (countArray)
    & Stream.maxThreads numCapabilities
    & Stream.aheadly
    & Stream.foldl' addCounts (False, (Counts 0 0 0 True))

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    name <- fmap head getArgs
    (_, Counts l w c _) <- wc name
    putStrLn $ show l ++ " " ++ show w ++ " " ++ show c ++ " " ++ name
