-- To run this program:
--
-- cabal run --flag fusion-plugin BytePairEncoder test-data.txt
--
module BytePairEncoder (main) where

import Data.Function ((&))
import qualified Data.Map as M
import qualified Data.Vector as V
import GHC.Word (Word8)
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
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  name <- fmap head getArgs
  let stream = File.read name
  mapping <- initializeSingleBytes stream
  print mapping
  indexStream <- Stream.toList . mapToIndexStream mapping $ stream
  print indexStream
