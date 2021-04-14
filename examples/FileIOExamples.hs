import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import Streamly.Internal.Data.Fold.Tee (Tee(..))
import qualified Streamly.Data.Array.Foreign as Array

import qualified Streamly.Internal.Data.Fold as Fold (classify)
import qualified Streamly.Internal.Data.Fold.Tee as Tee
import qualified Streamly.Internal.Data.Stream.IsStream as Stream (splitOnSeq)
import qualified Streamly.Internal.FileSystem.File as File
       (appendChunks, fromChunks, toBytes, toChunksWithBufferOf)

import Data.Char (ord)
import System.Environment (getArgs)

cat :: FilePath -> IO ()
cat src =
      File.fromChunks "/dev/stdout"
    $ File.toChunksWithBufferOf (256*1024) src

cp :: FilePath -> FilePath -> IO ()
cp src dst =
      File.fromChunks dst
    $ File.toChunksWithBufferOf (256*1024) src

append :: FilePath -> FilePath -> IO ()
append src dst =
      File.appendChunks dst
    $ File.toChunksWithBufferOf (256*1024) src

ord' :: Num a => Char -> a
ord' = (fromIntegral . ord)

wcl :: FilePath -> IO ()
wcl src = print =<< (Stream.length
    $ Stream.splitOnSuffix (== ord' '\n') Fold.drain
    $ File.toBytes src)

grepc :: String -> FilePath -> IO ()
grepc pat src = print . (subtract 1) =<< (Stream.length
    $ Stream.splitOnSeq (Array.fromList (map ord' pat)) Fold.drain
    $ File.toBytes src)

avgll :: FilePath -> IO ()
avgll src = print =<< (Stream.fold avg
    $ Stream.splitOnSuffix (== ord' '\n') Fold.length
    $ File.toBytes src)

    where

    avg = Tee.toFold $ (/)
            <$> Tee (toDouble Fold.sum)
            <*> Tee (toDouble Fold.length)
    toDouble = fmap (fromIntegral :: Int -> Double)

llhisto :: FilePath -> IO ()
llhisto src = print =<< (Stream.fold (Fold.classify Fold.length)
    $ Stream.map bucket
    $ Stream.splitOnSuffix (== ord' '\n') Fold.length
    $ File.toBytes src)
    where
    bucket n = let i = n `mod` 10 in if i > 9 then (9,n) else (i,n)

main :: IO ()
main = do
    src <- fmap head getArgs

    putStrLn "cat"    >> cat src              -- Unix cat program
    putStr "wcl "     >> wcl src              -- Unix wc -l program
    putStr "grepc "   >> grepc "aaaa" src     -- Unix grep -c program
    putStr "avgll "   >> avgll src            -- get average line length
    putStr "llhisto " >> llhisto src          -- get line length histogram
    putStr "cp "      >> cp src "dst-xyz.txt" -- Unix cp program
    putStr "append "  >> append src "dst-xyz.txt" -- Appending to file
