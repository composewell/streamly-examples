-- To run this program:
--
-- cabal run --flag fusion-plugin WordCountModular test-data.txt
--
module Main (main) where

import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Fold.Tee (Tee(..))
import System.Environment (getArgs)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Fold.Tee as Tee
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.File as File (read)

{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace c = uc == 0x20 || uc - 0x9 <= 4
  where uc = fromIntegral (ord c) :: Word

-------------------------------------------------------------------------------
-- Fold the bytes from an input file
-------------------------------------------------------------------------------

-- The fold accepts a stream of `Word8` and returns a value of type "a".
foldWith :: Fold IO Word8 a -> String -> IO a
foldWith f file =
    File.read file    -- Stream IO Word8
  & Stream.fold f     -- IO a

-------------------------------------------------------------------------------
-- Count Lines
-------------------------------------------------------------------------------

-- ASCII character 10 is newline
countl :: Int -> Word8 -> Int
countl n ch = if ch == 10 then n + 1 else n

nlines :: Monad m => Fold m Word8 Int
nlines = Fold.foldl' countl 0

-------------------------------------------------------------------------------
-- Count Words
-------------------------------------------------------------------------------

countw :: (Int, Bool) -> Word8 -> (Int, Bool)
countw (n, wasSpace) ch =
    if isSpace $ chr $ fromIntegral ch
    then (n, True)
    else (if wasSpace then n + 1 else n, False)

-- The fold accepts a stream of `Word8` and returns a word count (`Int`)
nwords :: Monad m => Fold m Word8 Int
nwords = fst <$> Fold.foldl' countw (0, True)

-------------------------------------------------------------------------------
-- Count Bytes, Lines, Words
-------------------------------------------------------------------------------

-- The fold accepts a stream of `Word8` and returns the three counts
countAll :: Fold IO Word8 (Int, Int, Int)
countAll = Tee.toFold $ (,,) <$> Tee Fold.length <*> Tee nlines <*> Tee nwords

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    name <- fmap head getArgs
    -- foldWith Fold.length name >>= print -- count bytes only
    -- foldWith nlines name >>= print      -- count lines only
    -- foldWith nwords name >>= print      -- count words only
    (c, l, w) <- foldWith countAll name    -- count all at once
    putStrLn $ show l ++ " " ++ show w ++ " " ++ show c ++ " " ++ name
