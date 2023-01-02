{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- IMPORTANT: Do not export anything other than "main" from this file. The
-- benchmark timings may change drastically otherwise.

module Main
    ( main
    )
where

import Data.Maybe (fromJust)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold (Tee(..))
import Streamly.Internal.Data.Fold (Fold(..), Step(..))
import Test.Tasty.Bench

import qualified Data.Char as Char
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as Fold (foldt', satisfy)
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Array as Array

-------------------------------------------------------------------------------
-- Monolithic fold - fastest, same as rust speeddate perf
-------------------------------------------------------------------------------

mkTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int
mkTime year month day hr mn sec = year + month + day + hr + mn + sec

{-# INLINE isDigit #-}
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

data FoldState =
      Year !Int
    | Month !Int !Int
    | Day !Int !Int
    | Hr !Int !Int
    | Min !Int !Int !Int
    | Sec !Int !Int !Int

{-# INLINE foldDateTime #-}
foldDateTime :: Array Char -> IO (Int, Int)
foldDateTime arr =
    Stream.fold t $ Stream.unfold Array.reader arr

    where

    t = Fold.foldt' step initial extract

    initial = Partial $ Year 0

    dec n ch = n * 10 + fromIntegral (Char.ord ch) - 48

    step (Year n) ch
      | isDigit ch = Partial $ Year (dec n ch)
      | ch == '-' = Partial $ Month (365 * n) 0
      | otherwise = error "parse error"
    step (Month y n) ch
      | isDigit ch = Partial $ Month y (dec n ch)
      | ch == '-' = Partial $ Day (y + n * 30) 0
      | otherwise = error "parse error"
    step (Day x n) ch
      | isDigit ch = Partial $ Day x (dec n ch)
      | ch == 'T' = Partial $ Hr (x + n) 0
      | otherwise = error "parse error"
    step (Hr d n) ch
      | isDigit ch = Partial $ Hr d (dec n ch)
      | ch == ':' = Partial $ Min d (n * 60) 0
      | otherwise = error "parse error"
    step (Min d m n) ch
      | isDigit ch = Partial $ Min d m (dec n ch)
      | ch == ':' = Partial $ Sec d ((m + n) * 60) 0
      | otherwise = error "parse error"
    step (Sec d s n) ch
      | isDigit ch = Partial $ Sec d s (dec n ch)
      | ch == 'Z' = Done (d, s + n)
      | otherwise = error "parse error"

    extract _ = error "incomplete"

-------------------------------------------------------------------------------
-- Modular applicative version - 4x slower
-------------------------------------------------------------------------------

{-# INLINE check #-}
check ::  (Char -> Bool) -> Fold m Char a -> Fold m Char a
check p = Fold.lmap (\x -> if p x then x else error "parse failed")

{-# INLINE decimal #-}
decimal :: Monad m  => Int -> Fold m Char Int
decimal n = Fold.take n (check isDigit (Fold.foldl' step 0))

    where

    step a c = a * 10 + fromIntegral (Char.ord c - 48)

{-# INLINE char #-}
char :: Monad m => Char -> Fold m Char Char
char c = fromJust <$> Fold.satisfy (== c)
{-
{-# NOINLINE _foldDateTimeAp #-}
_foldDateTimeAp :: Array Char -> IO Int
_foldDateTimeAp arr =
    let t =
                mkTime
            <$> Tee (decimal 4)  -- year
            <*  Tee (char '-')
            <*> Tee (decimal 2)  -- month
            <*  Tee (char '-')
            <*> Tee (decimal 2)  -- day
            <*  Tee (char 'T')
            <*> Tee (decimal 2)  -- hr
            <*  Tee (char ':')
            <*> Tee (decimal 2)  -- min
            <*  Tee (char ':')
            <*> Tee (decimal 2)  -- sec
            <*  Tee (char 'Z')
    in Stream.fold (unTee t) $ Stream.unfold Array.reader arr
-}
-------------------------------------------------------------------------------
-- Using foldBreak - slower than applicative
-------------------------------------------------------------------------------

_foldBreakDateTime :: Array Char -> IO Int
_foldBreakDateTime arr = do
    let s = Stream.unfold Array.reader arr
    (year, s1) <- Stream.foldBreak (decimal 4) s
    (_, s2) <- Stream.foldBreak (char '-') s1
    (month, s3) <- Stream.foldBreak (decimal 2) s2
    (_, s4) <- Stream.foldBreak (char '-') s3
    (day, s5) <- Stream.foldBreak (decimal 2) s4
    (_, s6) <- Stream.foldBreak (char 'T') s5
    (hr, s7) <- Stream.foldBreak (decimal 2) s6
    (_, s8) <- Stream.foldBreak (char ':') s7
    (mn, s9) <- Stream.foldBreak (decimal 2) s8
    (_, s10) <- Stream.foldBreak (char ':') s9
    (sec, s11) <- Stream.foldBreak (decimal 2) s10
    (_, _) <- Stream.foldBreak (char 'Z') s11
    return (year + month + day + hr + mn + sec)

-------------------------------------------------------------------------------
-- Using parseBreak - slower than applicative
-------------------------------------------------------------------------------

_parseBreakDateTime :: Array Char -> IO Int
_parseBreakDateTime arr = do
    let s = Stream.unfold Array.reader arr
    (Right year, s1) <- Stream.parseBreak (Parser.fromFold $ decimal 4) s
    (_, s2) <- Stream.parseBreak (Parser.fromFold $ char '-') s1
    (Right month, s3) <- Stream.parseBreak (Parser.fromFold $ decimal 2) s2
    (_, s4) <- Stream.parseBreak (Parser.fromFold $ char '-') s3
    (Right day, s5) <- Stream.parseBreak (Parser.fromFold $ decimal 2) s4
    (_, s6) <- Stream.parseBreak (Parser.fromFold $ char 'T') s5
    (Right hr, s7) <- Stream.parseBreak (Parser.fromFold $ decimal 2) s6
    (_, s8) <- Stream.parseBreak (Parser.fromFold $ char ':') s7
    (Right mn, s9) <- Stream.parseBreak (Parser.fromFold $ decimal 2) s8
    (_, s10) <- Stream.parseBreak (Parser.fromFold $ char ':') s9
    (Right sec, s11) <- Stream.parseBreak (Parser.fromFold $ decimal 2) s10
    (_, _) <- Stream.parseBreak (Parser.fromFold $ char 'Z') s11
    return (year + month + day + hr + mn + sec)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- IMPORTANT: Enable only one benchmark at a time, the benchmark timings may
-- change drastically otherwise.
--
timeBench :: Benchmark
timeBench =
    let !(arr :: Array Char) = Array.fromListN 20 "2000-01-01T00:02:03Z"
    in bgroup "parseDateTime"
        [ bench "fold monolithic" $ nfIO $ foldDateTime arr
        -- , bench "fold applicative" $ nfIO $ _foldDateTimeAp arr
        -- , bench "foldBreak" $ nfIO $ _foldBreakDateTime arr
        -- , bench "parseBreak" $ nfIO $ _parseBreakDateTime arr
        ]

main :: IO ()
main = defaultMain [timeBench]
