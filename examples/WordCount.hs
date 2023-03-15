-- To run this program:
--
-- cabal run --flag fusion-plugin WordCount test-data.txt
--
module WordCount (main, count, Counts(..), isSpace) where

import Data.Char (ord)
import Data.Function ((&))
import System.Environment (getArgs)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Unicode.Stream as Stream

-------------------------------------------------------------------------------
-- C compatible isSpace
-------------------------------------------------------------------------------

{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace c = uc == 0x20 || uc - 0x9 <= 4
  where uc = fromIntegral (ord c) :: Word

-------------------------------------------------------------------------------
-- Counting
-------------------------------------------------------------------------------

-- Counts lines words chars lastCharWasSpace
data Counts = Counts !Int !Int !Int !Bool deriving Show

{-# INLINE count #-}
count :: Counts -> Char -> Counts
count (Counts l w c wasSpace) ch =
    let l1 = if ch == '\n' then l + 1 else l
        (w1, wasSpace1) =
            if isSpace ch
            then (w, True)
            else (if wasSpace then w + 1 else w, False)
    in Counts l1 w1 (c + 1) wasSpace1

wc :: String -> IO Counts
wc file =
      File.read file                                      -- Stream IO Word8
    & Stream.decodeLatin1                                 -- Stream IO Char
 -- & Stream.decodeUtf8                                   -- Stream IO Char
    & Stream.fold (Fold.foldl' count (Counts 0 0 0 True)) -- IO Counts

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    name <- fmap head getArgs
    Counts l w c _ <- wc name
    putStrLn $ show l ++ " " ++ show w ++ " " ++ show c ++ " " ++ name
