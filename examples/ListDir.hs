module Main (main) where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Streamly.Prelude (SerialT)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
       (iterateMapLeftsWith)
import qualified Streamly.Internal.FileSystem.Dir as Dir (toEither)

-- Lists a dir as a stream of (Either Dir File)
listDir :: String -> SerialT IO (Either String String)
listDir dir =
      Dir.toEither dir               -- SerialT IO (Either String String)
    & Stream.map (bimap mkAbs mkAbs) -- SerialT IO (Either String String)

    where mkAbs x = dir ++ "/" ++ x

-- | List the current directory recursively using concurrent processing
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let start = Stream.fromPure (Left ".")
    Stream.iterateMapLeftsWith Stream.ahead listDir start
        & Stream.mapM_ print
