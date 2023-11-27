import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT(..), get, put)
import Data.Function ((&))
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), openFile, hClose)
import Streamly.Data.Stream (Stream)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as Stream (refoldMany)
import qualified Streamly.Internal.Data.Refold.Type as Refold (take)
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.FileSystem.Handle as Handle (writer)

newHandle :: StateT (Maybe (Handle, Int)) IO Handle
newHandle = do
    old <- get
    idx <- case old of
            Nothing -> return 0
            Just (h, i) -> liftIO (hClose h) >> return (i + 1)
    h <- liftIO $ openFile ("output-" ++ show idx ++ ".txt") WriteMode
    put (Just (h, idx))
    return h

-- XXX reduce the input stream to a stream of file names
-- The fold can return the file name/handle after it is done.
-- similarly the files can written to directories and we can generate a stream
-- of directory names.
splitFile :: Handle -> IO ()
splitFile inHandle =
      (Handle.read inHandle :: Stream IO Word8) -- Stream IO Word8
    & Stream.liftInner                   -- Stream (StateT (Maybe (Handle, Int)) IO) Word8
    -- Stream (StateT (Maybe (Handle, Int)) IO) ()
    & Stream.refoldMany (Refold.take (180 * mb) Handle.writer) newHandle
    & Stream.runStateT (return Nothing)  -- Stream IO (Maybe (Handle, Int), ())
    & fmap snd                           -- Stream IO ()
    & Stream.fold Fold.drain             -- Stream IO ()

    where

    mb = 1024 * 1024

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode
    splitFile src
