-- To run this program:
--
-- cabal run --flag fusion-plugin BytePairEncoder test-data.txt
--
module BytePairEncoder (main) where

import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Data.List (maximumBy)
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Data.Vector as V
import GHC.Word (Word8)
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.File as File
import System.Environment (getArgs)

-------------------------------------------------------------------------------
-- Byte indexing and text representation
-------------------------------------------------------------------------------

-- Stores byte-sequence-to-index mapping and index-to-text mapping
data ByteMappings = ByteMappings
  { byteToIndex :: !(M.Map Word8 Int), -- Maps bytes to unique indices
    seqToIndex :: !(M.Map (V.Vector Word8) Int), -- Maps sequences of bytes to unique indices
    indexToText :: !(M.Map Int String), -- Maps indices to text representation
    nextIndex :: !Int -- Next available index
  }

instance Show ByteMappings where
  show (ByteMappings b2i _ i2t nidx) =
    "ByteMappings:\n"
      ++ "byteToIndex = "
      ++ show b2i
      ++ "\n"
      ++ "indexToText = "
      ++ show i2t
      ++ "\n"
      ++ "nextIndex = "
      ++ show nidx

{-# INLINE initializeSingleBytes #-}
initializeSingleBytes :: (Monad m) => Stream m Word8 -> m ByteMappings
initializeSingleBytes stream = do
  -- Collect unique bytes and create initial mappings
  uniqueBytes <-
    stream
      & Stream.fold (Fold.foldl' (\m b -> M.insert b () m) M.empty)

  let bytes = V.fromList $ M.keys uniqueBytes
      indices = [0 .. (V.length bytes - 1)]
      b2i = M.fromList $ zip (V.toList bytes) indices
      s2i = M.fromList $ zip (map V.singleton (V.toList bytes)) indices
      i2t = M.fromList $ zip indices (map ((: []) . toEnum . fromIntegral) (V.toList bytes))

  return $ ByteMappings b2i s2i i2t (length indices)

{-# INLINE mapToIndexStream #-}
mapToIndexStream :: (Monad m) => ByteMappings -> Stream m Word8 -> Stream m Int
mapToIndexStream mapping = fmap (\k -> M.findWithDefault (-1) k (byteToIndex mapping))

-------------------------------------------------------------------------------
-- Count and merge most frequent pairs
-------------------------------------------------------------------------------

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

-- Stores pair frequencies for merging
type PairFrequencies = M.Map (Int, Int) Int

{-# INLINE countPairs #-}
countPairs :: (MonadIO m) => Stream m Int -> m PairFrequencies
countPairs stream =
  stream
    & Stream.chunksOf 2
    & Stream.fold (Fold.foldl' addPair M.empty)
  where
    addPair acc chunk =
      case Array.toList chunk of
        [b1, b2] -> M.insertWith (+) (b1, b2) 1 acc
        _ -> acc

{-# INLINE mergeMostFrequentPair #-}
mergeMostFrequentPair :: ByteMappings -> PairFrequencies -> ByteMappings
mergeMostFrequentPair mappings@(ByteMappings b2i s2i i2t nidx) freqs =
  if M.null freqs
    then mappings
    else
      let ((b1, b2), _) = maximumBy (comparing snd) (M.toList $ freqs)
          text1 = M.findWithDefault "?" b1 i2t
          text2 = M.findWithDefault "?" b2 i2t
          newToken = text1 ++ text2
          bytes = V.fromList $ map charToWord8 newToken
       in ByteMappings
            b2i
            (M.insert bytes nidx s2i)
            (M.insert nidx newToken i2t)
            (nidx + 1)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  name <- fmap head getArgs
  let stream = File.read name
  mapping <- initializeSingleBytes stream
  print mapping
  let byteIndexStream = mapToIndexStream mapping stream
  indexStream <- Stream.toList byteIndexStream
  print indexStream
  freqs <- countPairs byteIndexStream
  let mergedMappings = mergeMostFrequentPair mapping freqs
  print mergedMappings
