{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- compile with:
-- ghc -O2 -fspec-constr-recursive=10 -fmax-worker-args=16 word-classifier.hs
--
import Data.Foldable
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.Hashable
import Data.IORef
import Foreign.Storable (Storable(..))
import System.Environment (getArgs)

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as Fold
   (rollingHash, rollingHashWithSalt)
import qualified Streamly.Internal.Data.Unfold as Unfold (fold)
import qualified Streamly.Internal.FileSystem.File as File (toBytes)
import qualified Streamly.Internal.Unicode.Stream as Unicode (words)
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Unicode

instance (Enum a, Storable a) => Hashable (Array.Array a) where
    hash arr = fromIntegral $ runIdentity $ Unfold.fold Fold.rollingHash Array.read arr
    hashWithSalt salt arr = fromIntegral $ runIdentity $
        Unfold.fold (Fold.rollingHashWithSalt $ fromIntegral salt) Array.read arr

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

    -- Write the stream to a hashmap consisting of word counts
    mp <-
        let
            alter Nothing    = Just <$> newIORef (1 :: Int)
            alter (Just ref) = modifyIORef' ref (+ 1) >> return (Just ref)
        in File.toBytes inFile         -- SerialT IO Word8
         & Unicode.decodeLatin1        -- SerialT IO Char
         & Stream.map toLower          -- SerialT IO Char
         & Unicode.words Fold.toList   -- SerialT IO String
         & Stream.filter (all isAlpha) -- SerialT IO String
         & Stream.foldlM' (flip (Map.alterF alter)) (return Map.empty) -- IO (Map String (IORef Int))

    -- Print the top hashmap entries
    counts <-
        let readRef (w, ref) = do
                cnt <- readIORef ref
                return (w, cnt)
         in Map.toList mp
          & mapM readRef

    traverse_ print $ List.sortOn (Ord.Down . snd) counts
                    & List.take 25
