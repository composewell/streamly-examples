-- Introductory example programs

import Control.Concurrent (threadDelay)
import Data.Char (ord, isSpace)
import Data.Functor.Identity (Identity(..))
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (stdout)

import Streamly.Prelude (SerialT)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Fold.Tee (Tee(..))
import Streamly.Data.Unfold (Unfold)

import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Prelude as Stream
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Unicode.Stream as Unicode

import qualified Streamly.Internal.Data.Unfold as Unfold (enumerateFromToIntegral)
import qualified Streamly.Internal.Data.Fold as Fold (classify)
import qualified Streamly.Internal.FileSystem.File as File (toBytes)

-------------------------------------------------------------------------------
-- Simple loops
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Nested loops
-------------------------------------------------------------------------------

-- | Nested looping example using unfolds. This is the most efficient way to do
-- nested loops (or cross product) when the two streams do not depend on each
-- other. The loops fuse completely generating code equivalent to C.
crossProduct :: (Int,Int) -> (Int,Int) -> Identity Int
crossProduct (from1,to1) (from2,to2) =
    let
        -- cross multiply src1 and src2 e.g.
        -- if src1 = [1,2], and src2 = [3,4] then src1 x src2 =
        -- [(1*3),(1*4),(2*3),(2*4)]
        xmult :: Unfold Identity (Int,Int) Int
        xmult = Unfold.crossWith (*) src1 src2

     in Stream.unfold xmult (from1,from2)  -- SerialT Identity Int
            & Stream.fold Fold.sum         -- Identity Int

    where

    -- The input to the unfold is (from1,from2)
    -- Generate a stream from from1..to1
    src1 :: Monad m => Unfold m (Int,Int) Int
    src1 = Unfold.lmap fst $ Unfold.enumerateFromToIntegral to1

    -- The input to the unfold is (from1,from2)
    -- Generate a stream from from2..to2
    src2 :: Monad m => Unfold m (Int,Int) Int
    src2 = Unfold.lmap snd $ Unfold.enumerateFromToIntegral to2

-- | Nested looping similar to 'cross' above but more general and less
-- efficient. The second stream may depend on the first stream. The loops
-- cannot fuse completely.
--
nestedLoops :: SerialT IO ()
nestedLoops = do
    x <- Stream.fromList [3,4 :: Int]
    y <- Stream.fromList [1..x]
    Stream.yieldM $ print (x, y)

-------------------------------------------------------------------------------
-- Text processing
-------------------------------------------------------------------------------

-- | Find average line length for lines in a text file
avgLineLength :: IO Double
avgLineLength =
      File.toBytes "input.txt"                    -- SerialT IO Word8
    & Stream.splitOnSuffix isNewLine Fold.length  -- SerialT IO Int
    & Stream.fold avg                             -- IO Double

    where

    isNewLine :: Word8 -> Bool
    isNewLine = (== (fromIntegral . ord) '\n')

    toDouble :: Fold IO Int Int -> Fold IO Int Double
    toDouble = fmap (fromIntegral :: Int -> Double)

    avg :: Fold IO Int Double
    avg = toFold $ (/)
            <$> Tee (toDouble Fold.sum)
            <*> Tee (toDouble Fold.length)

-- | Read text from a file and generate a histogram of line length
lineLengthHistogram :: IO (Map Int Int)
lineLengthHistogram =
      File.toBytes "input.txt"                   -- SerialT IO Word8
    & Stream.splitOnSuffix isNewLine Fold.length -- SerialT IO Int
    & Stream.map bucket                          -- SerialT IO (Int, Int)
    & Stream.fold (Fold.classify Fold.length)    -- IO (Map Int Int)

    where

    isNewLine :: Word8 -> Bool
    isNewLine = (== (fromIntegral . ord) '\n')

    bucket :: Int -> (Int, Int)
    bucket n = let i = n `mod` 10 in if i > 9 then (9,n) else (i,n)

-- | Read text from a file and generate a histogram of word length
wordLengthHistogram :: IO (Map Int Int)
wordLengthHistogram =
      File.toBytes "input.txt"                -- SerialT IO Word8
    & Unicode.decodeLatin1                    -- SerialT IO Char
    & Stream.wordsBy isSpace Fold.length      -- SerialT IO Int
    & Stream.map bucket                       -- SerialT IO (Int, Int)
    & Stream.fold (Fold.classify Fold.length) -- IO (Map (Int, Int))

    where

    bucket :: Int -> (Int, Int)
    bucket n = let i = n `mod` 10 in if i > 9 then (9,n) else (i,n)

-------------------------------------------------------------------------------
-- Network/Concurrency
-------------------------------------------------------------------------------

-- Simulate network/db query by adding a delay
fetch :: String -> IO (String, String)
fetch w = threadDelay 1000000 >> return (w,w)

wordList :: [String]
wordList = ["cat", "dog", "mouse"]

meanings :: [IO (String, String)]
meanings = map fetch wordList

-- | Fetch word meanings for words in 'wordList'. All searches are performed
-- concurrently.
--
getWords :: IO ()
getWords =
      Stream.fromListM meanings                     -- AheadT  IO (String, String)
    & Stream.fromAhead                              -- SerialT IO (String, String)
    & Stream.map show                               -- SerialT IO String
    & unlinesBy "\n"                                -- SerialT IO String
    & Stream.map Array.fromList                     -- SerialT IO (Array Word8)
    & Stream.fold (Handle.writeChunks stdout)       -- IO ()

    where unlinesBy = Stream.intercalateSuffix (Unfold.function id)

main :: IO ()
main = do
    cmd <- fmap head getArgs

    case cmd of
        "sumInt" -> putStrLn "sumInt" >> print (runIdentity sumInt)
        "sumInt1" -> putStrLn "sumInt1" >> print (runIdentity sumInt1)
        "crossProduct" -> do
            putStrLn "crossProduct"
            print $ runIdentity $ crossProduct (1,1000) (1000,2000)
        "nestedLoops" -> putStrLn "nestedLoops" >> Stream.drain nestedLoops
        "avgLineLength" -> do
            putStrLn "avgLineLength"
            avgLineLength >>= print
        "lineLengthHistogram" -> do
            putStrLn "lineLengthHistogram"
            lineLengthHistogram >>= print
        "wordLengthHistogram" -> do
            putStrLn "wordLengthHistogram"
            wordLengthHistogram >>= print
        "getWords" -> do
            putStrLn "getWords"
            getWords >>= print
        _ -> putStrLn $ "Unknown command: " ++ cmd
