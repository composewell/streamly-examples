-- To run this program:
--
-- cabal run --flag fusion-plugin BytePairEncoder test-data.txt
--
module BytePairEncoder (main) where

import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Data.List (maximumBy)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
import qualified Data.Vector as V
import GHC.Word (Word8)
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.File as File
import Streamly.Internal.Data.Pipe (Pipe (..), Step (..))
import Streamly.Internal.Data.Stream (pipe)
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

mostFrequentPair :: PairFrequencies -> ((Int, Int), Int)
mostFrequentPair = maximumBy (comparing snd) . M.toList

updateMappings :: ByteMappings -> (Int, Int) -> ByteMappings
updateMappings (ByteMappings b2i s2i i2t nidx) (i1, i2) =
  let text1 = M.findWithDefault "?" i1 i2t
      text2 = M.findWithDefault "?" i2 i2t
      newToken = text1 ++ text2
      bytes = V.fromList $ map charToWord8 newToken
   in ByteMappings
        b2i
        (M.insert bytes nidx s2i)
        (M.insert nidx newToken i2t)
        (nidx + 1)

{-# INLINE replaceMostFrequentPair #-}
replaceMostFrequentPair :: (Monad m) => (Int, Int) -> Int -> Pipe m Int Int
replaceMostFrequentPair (i1, i2) nidx = Pipe consume produce False
  where
    consume False i | i == i1 = return $ SkipC True -- found first index
    consume False i = return $ YieldP Nothing i -- first index not found
    consume True i | i == i2 = return $ YieldP Nothing nidx -- found second index
    consume True i | i == i1 = return $ YieldC True i1 -- encountered first index again
    consume True i = return $ YieldP (Just i) i1 -- fallback
    produce Nothing = return $ SkipC False
    produce (Just i) = return $ YieldC False i

-------------------------------------------------------------------------------
-- Build BPE mapping
-------------------------------------------------------------------------------

mergeUntil :: (MonadIO m) => Int -> ByteMappings -> Stream m Int -> m ByteMappings
mergeUntil threshold mapping stream = do
  freqs <- countPairs stream
  let (i1, i2) = fst . mostFrequentPair $ freqs
      updatedMapping = updateMappings mapping (i1, i2)
      replacePipe = replaceMostFrequentPair (i1, i2) (nextIndex updatedMapping - 1)
      newStream = pipe replacePipe stream
  if nextIndex updatedMapping >= threshold
    then return updatedMapping
    else mergeUntil threshold updatedMapping newStream

-- | Produce an (infinite) stream of updated ByteMappings.
mergedMappingsStream :: (MonadIO m) => ByteMappings -> Stream.Stream m Int -> Stream.Stream m ByteMappings
mergedMappingsStream initMapping initStream =
  Stream.unfoldrM step (initMapping, initStream)
  where
    step (mapping, stream) = do
      freqs <- countPairs stream
      let (i1, i2) = fst $ mostFrequentPair freqs
          updatedMapping = updateMappings mapping (i1, i2)
          newIdx = nextIndex updatedMapping - 1
          replacePipe = replaceMostFrequentPair (i1, i2) newIdx
          newStream = pipe replacePipe stream
      return $ Just (updatedMapping, (updatedMapping, newStream))

-------------------------------------------------------------------------------
-- Tokenize text
-------------------------------------------------------------------------------

word8ToChar :: Word8 -> Char
word8ToChar = toEnum . fromIntegral

-- | Find token from looking up bytes from mapping
findTokenFromBytes :: V.Vector Word8 -> ByteMappings -> Maybe String
findTokenFromBytes bytes mapping = do
  idx <- M.lookup bytes (seqToIndex mapping)
  return $ M.findWithDefault (error "Missing token text") idx (indexToText mapping)

-- | Find longest token from looking up bytes from mapping
findLongestTokenFromBytes :: (Monad m) => V.Vector Word8 -> ByteMappings -> m (Maybe String)
findLongestTokenFromBytes bytes mapping =
  let candidates = Stream.takeWhile (not . V.null) $ Stream.iterate V.init bytes
      tokens = Stream.mapMaybe (`findTokenFromBytes` mapping) candidates
  in Stream.fold Fold.one tokens

-- | 'tokenize' consumes a stream of bytes and produces tokens.
--
-- The tokens are determined by the ByteMappings. The pipe's state is a tuple
-- consisting of the current byte buffer, the last valid candidate token, and its byte count.
--
-- On each new byte:
--   • Extend the buffer.
--   • If the extended buffer is in the mapping (i.e. is a valid token) update the candidate
--   • Otherwise, emit the candidate token (the longest match so far),
--     reset the state (starting with the current byte), and continue.
{-# INLINE tokenize #-}
tokenize :: (Monad m) => ByteMappings -> Pipe m Word8 String
tokenize mapping = Pipe consume produce (V.empty, "", 0)
  where
    -- State: (current buffer, candidate token, candidate byte count)
    consume ::
      (Monad m) =>
      (V.Vector Word8, String, Int) -> -- current state
      Word8 -> -- new byte
      m (Step (V.Vector Word8, String, Int) () String)
    consume (buf, cand, candCount) byte =
      let newBuf = V.snoc buf byte
       in case findTokenFromBytes newBuf mapping of
            -- Update with new candidate token and continue consuming
            Just newCand -> return $ SkipC (newBuf, newCand, length newCand)
            -- Extended buffer not valid is not a valid key
            -- Continue with existing candidate
            -- if the buffer length has exceeded the threshold
            -- Yield the candidate and reset state
            Nothing ->
              if V.length newBuf <= 20
                then return $ SkipC (newBuf, cand, candCount)
                else do
                  let rest = V.drop candCount newBuf
                  longestToken <- findLongestTokenFromBytes rest mapping
                  let nextCand = fromMaybe "" longestToken
                  let nextCandCount = length nextCand
                  return $ YieldC (rest, nextCand, nextCandCount) cand

    -- When input is exhausted, if the buffer is non-empty emit the candidate.
    produce = undefined

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  name <- fmap head getArgs
  let stream = File.read name
  mapping <- initializeSingleBytes stream
  print mapping
  let indexStream = mapToIndexStream mapping stream
      mappingStream = mergedMappingsStream mapping indexStream
      printMappingStream = Stream.trace print mappingStream
  maybeMapping <- Stream.fold (Fold.index 100) printMappingStream
  let m2 = fromJust maybeMapping
  print m2
  let tokenStream = pipe (tokenize m2) stream
  Stream.fold (Fold.drainMapM (\s -> putStr (s ++ ","))) tokenStream
