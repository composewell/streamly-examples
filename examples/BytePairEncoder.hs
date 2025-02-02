-- To run this program:
--
-- cabal run --flag fusion-plugin BytePairEncoder test-data.txt
--
module BytePairEncoder (main) where

import Data.Function ((&))
import qualified Data.Map as M
import GHC.Word (Word8)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.File as File
import System.Environment (getArgs)

-------------------------------------------------------------------------------
-- Byte indexing and text representation
-------------------------------------------------------------------------------

-- Stores byte-to-index mapping and index-to-text mapping
data ByteMappings = ByteMappings
  { byteToIndex :: !(M.Map Word8 Int), -- Maps bytes to unique indices
    indexToText :: !(M.Map Int String) -- Maps indices to text representation
  }
  deriving (Show)

{-# INLINE assignIndex #-}
assignIndex :: ByteMappings -> Word8 -> ByteMappings
assignIndex (ByteMappings b2i i2t) byte =
  case M.lookup byte b2i of
    Just _ -> ByteMappings b2i i2t -- byte already indexed
    Nothing ->
      let nextIndex = M.size b2i -- next available index
          byteText = [toEnum (fromIntegral byte) :: Char] -- convert byte to ASCII char
       in ByteMappings
            (M.insert byte nextIndex b2i)
            (M.insert nextIndex byteText i2t)

indexBytes :: String -> IO ByteMappings
indexBytes file =
  File.read file -- Stream IO Word8
    & Stream.fold (Fold.foldl' assignIndex initialMappings) -- IO ByteMappings
  where
    initialMappings = ByteMappings M.empty M.empty

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  name <- fmap head getArgs
  mappings <- indexBytes name
  print mappings -- Print both mappings
