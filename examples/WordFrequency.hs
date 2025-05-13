{-# LANGUAGE FlexibleInstances, CPP #-}

{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.Hashable (Hashable(..))
import System.Environment (getArgs)
import Streamly.Data.Array (Unbox)

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold.Prelude as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.FileSystem.FileIO as File
import qualified Streamly.Unicode.Stream as Unicode

hashArray :: (Unbox a, Integral b, Num c) =>
    Fold.Fold Identity a b -> Array.Array a -> c
hashArray f arr =
      Array.read arr
    & Stream.fold f
    & fromIntegral . runIdentity

#if MIN_VERSION_hashable(1,4,0)
instance (Eq a, Enum a, Unbox a) => Hashable (Array.Array a) where
#else
instance (Enum a, Unbox a) => Hashable (Array.Array a) where
#endif
    hash = hashArray Fold.rollingHash

    hashWithSalt salt =
        hashArray (Fold.rollingHashWithSalt (fromIntegral salt))

{-# INLINE toLower #-}
toLower :: Char -> Char
toLower c
  | uc >= 0x61 && uc <= 0x7a = c
  | otherwise = Char.toLower c
  where
    uc = fromIntegral (Char.ord c) :: Word

{-# INLINE isAlpha #-}
isAlpha :: Char -> Bool
isAlpha c
  | uc >= 0x61 && uc <= 0x7a = True
  | otherwise = Char.isAlpha c
  where
    uc = fromIntegral (Char.ord c) :: Word

main :: IO ()
main = do
    inFile <- fmap head getArgs
    inFileP <- Path.fromString inFile

    let counter = Fold.foldl' (\n _ -> n + 1) (0 :: Int)
        classifier = Fold.toHashMapIO id counter
    -- Write the stream to a hashmap consisting of word counts
    mp <-
        File.read inFileP                      -- Stream IO Word8
         & Unicode.decodeLatin1                -- Stream IO Char
         & fmap toLower                        -- Stream IO Char
         & Stream.wordsBy isSpace Fold.toList  -- Stream IO String
         & Stream.filter (all isAlpha)         -- Stream IO String
         & Stream.fold classifier              -- IO (HashMap String Int)

    traverse_ print
        $ List.sortOn (Ord.Down . snd) (Map.toList mp)
        & List.take 25
