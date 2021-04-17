-- To run this program:
--
-- cabal run --flag fusion-plugin WordCountModular test-data.txt
--
import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Fold.Tee (Tee(..))
import System.Environment (getArgs)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.FileSystem.File as File (toBytes)
import qualified Streamly.Internal.Data.Fold.Tee as Tee (toFold)
import qualified Streamly.Prelude as Stream

{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace c = uc == 0x20 || uc - 0x9 <= 4
  where uc = fromIntegral (ord c) :: Word

-------------------------------------------------------------------------------
-- Count Bytes
-------------------------------------------------------------------------------

_wcb :: String -> IO Int
_wcb file =
    File.toBytes file        -- SerialT IO Word8
  & Stream.fold Fold.length  -- IO Int

-------------------------------------------------------------------------------
-- Count Lines
-------------------------------------------------------------------------------

countl :: Int -> Word8 -> Int
countl n ch = if ch == 10 then n + 1 else n

-- The fold accepts a stream of `Word8` and returns a line count (`Int`).
nlines :: Monad m => Fold m Word8 Int
nlines = Fold.foldl' countl 0

_wcl :: String -> IO Int
_wcl file =
    File.toBytes file  -- SerialT IO Word8
  & Stream.fold nlines -- IO Int

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

_wcw :: String -> IO Int
_wcw file =
    File.toBytes file   -- SerialT IO Word8
  & Stream.fold nwords  -- IO Int

-------------------------------------------------------------------------------
-- Count Bytes, Lines, Words
-------------------------------------------------------------------------------

countAll :: Fold IO Word8 (Int, Int, Int)
countAll = Tee.toFold $ (,,) <$> Tee Fold.length <*> Tee nlines <*> Tee nwords

wc :: String -> IO (Int, Int, Int)
wc file =
    File.toBytes file    -- SerialT IO Word8
  & Stream.fold countAll -- IO (Int, Int, Int)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    name <- fmap head getArgs
    -- _wcb name >>= print
    -- _wcl name >>= print
    -- _wcw name >>= print
    -- wc name >>= print
    (c, l, w) <- wc name
    putStrLn $ show l ++ " " ++ show w ++ " " ++ show c ++ " " ++ name
