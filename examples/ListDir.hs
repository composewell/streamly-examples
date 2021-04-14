module Main (main) where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
       (iterateMapLeftsWith)
import qualified Streamly.Internal.FileSystem.Dir as Dir (toEither)

-- | List the current directory recursively using concurrent processing
--
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    Stream.mapM_ print $ Stream.iterateMapLeftsWith Stream.ahead listDir (Stream.yield $ (Left "."))

    where

    listDir dir =
          Dir.toEither dir            -- SerialT IO (Either String String)
        & Stream.map (bimap prefix prefix) -- SerialT IO (Either String String)
        where prefix x = dir ++ "/" ++ x
