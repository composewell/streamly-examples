-- Introductory example programs

import Control.Concurrent (threadDelay)
import Data.Char (ord, isSpace)
import Data.Functor.Identity (Identity(..))
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (stdout)

import Streamly.Data.Fold (Fold)
import Streamly.Data.Fold.Tee (Tee(..))
import Streamly.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)
import Streamly.Internal.Data.Stream.Cross (CrossStream (..))

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Unfold as Unfold (enumerateFromToIntegral)
import qualified Streamly.Internal.Data.Fold.Extra as Fold (classify)
import qualified Streamly.Internal.FileSystem.File as File (read)
import qualified Streamly.Internal.Data.Stream as Stream (catRights)

-------------------------------------------------------------------------------
-- Simple loops
-------------------------------------------------------------------------------

-- | Sum a list of Int
sumInt :: Identity Int
sumInt =
      Stream.unfold Unfold.fromList [1..10] -- Stream Identity Int
    & Stream.fold Fold.sum                  -- Identity Int

-- | Sum a list of Int
sumInt1 :: Identity Int
sumInt1 =
      Stream.fromList [1..10]       -- Stream Identity Int
    & Stream.fold Fold.sum          -- Identity Int

-------------------------------------------------------------------------------
-- Nested loops
-------------------------------------------------------------------------------

-- | Nested looping example using unfolds. This is the most efficient way to do
-- nested loops (or cross product) when the two streams do not depend on each
-- other. The loops fuse completely generating code equivalent to C.
crossProduct :: (Int,Int) -> (Int,Int) -> Identity Int
crossProduct range1 range2 =
    let
        -- cross multiply src1 and src2 e.g.
        -- if src1 = [1,2], and src2 = [3,4] then src1 x src2 =
        -- [(1*3),(1*4),(2*3),(2*4)]
        xmult :: Unfold Identity ((Int, Int), (Int, Int)) Int
        xmult =
            Unfold.crossWith (*)
                  (Unfold.lmap fst Unfold.enumerateFromToIntegral)
                  (Unfold.lmap snd Unfold.enumerateFromToIntegral)

     in Stream.unfold xmult (range1,range2) -- Stream Identity Int
            & Stream.fold Fold.sum          -- Identity Int

-- | Nested looping similar to 'cross' above but more general and less
-- efficient. The second stream may depend on the first stream. The loops
-- cannot fuse completely.
--
nestedLoops :: CrossStream IO ()
nestedLoops = do
    x <- CrossStream $ Stream.fromList [3,4 :: Int]
    y <- CrossStream $ Stream.fromList [1..x]
    CrossStream $ Stream.fromEffect $ print (x, y)

-------------------------------------------------------------------------------
-- Text processing
-------------------------------------------------------------------------------

splitOn :: Monad m => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitOn p f = Stream.foldMany (Fold.takeEndBy_ p f)

-- | Find average line length for lines in a text file
avgLineLength :: IO Double
avgLineLength =
      File.read "input.txt"                    -- Stream IO Word8
    & splitOn isNewLine Fold.length               -- Stream IO Int
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
      File.read "input.txt"                   -- Stream IO Word8
    & splitOn isNewLine Fold.length              -- Stream IO Int
    & fmap bucket                                -- Stream IO (Int, Int)
    & Stream.fold (Fold.classify Fold.length)    -- IO (Map Int Int)

    where

    isNewLine :: Word8 -> Bool
    isNewLine = (== (fromIntegral . ord) '\n')

    bucket :: Int -> (Int, Int)
    bucket n = let i = n `mod` 10 in if i > 9 then (9,n) else (i,n)

-- | Read text from a file and generate a histogram of word length
wordLengthHistogram :: IO (Map Int Int)
wordLengthHistogram =
      File.read "input.txt"                -- Stream IO Word8
    & Unicode.decodeLatin1                    -- Stream IO Char
    & Stream.parseMany wordLen                -- Stream IO Int
    & Stream.catRights
    & fmap bucket                             -- Stream IO (Int, Int)
    & Stream.fold (Fold.classify Fold.length) -- IO (Map (Int, Int))

    where

    bucket :: Int -> (Int, Int)
    bucket n = let i = n `mod` 10 in if i > 9 then (9,n) else (i,n)

    wordLen = Parser.wordBy isSpace Fold.length

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
      Stream.fromList meanings                -- Stream IO (IO (String, String))
    & Concur.parSequence
        (Concur.ordered True)                 -- Stream IO (String, String)
    & fmap show                               -- Stream IO String
    & unlinesBy "\n"                          -- Stream IO String
    & fmap Array.fromList                     -- Stream IO (Array Word8)
    & Stream.fold (Handle.writeChunks stdout) -- IO ()

    where unlinesBy = Stream.intercalateSuffix (Unfold.function id)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- Usage example: Intro sumInt
--
-- Some examples (e.g. avgLineLength) require a text file "input.txt" in
-- the current directory.
--
main :: IO ()
main = do
    cmd <- fmap head getArgs

    case cmd of
        "sumInt" -> do
            putStrLn "sumInt"
            print (runIdentity sumInt)
        "sumInt1" -> do
            putStrLn "sumInt1"
            print (runIdentity sumInt1)
        "crossProduct" -> do
            putStrLn "crossProduct"
            print $ runIdentity $ crossProduct (1,1000) (1000,2000)
        "nestedLoops" -> do
            putStrLn "nestedLoops"
            Stream.fold Fold.drain $ unCrossStream nestedLoops
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
