import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT(..), get, put)
import Data.Function ((&))
import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), openFile, hClose)

import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.Data.Stream.IsStream as Stream (chunksOf2, evalStateT)
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.FileSystem.Handle as Handle (write2)

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
      Stream.unfold Handle.read inHandle -- SerialT IO Word8
    & Stream.liftInner -- SerialT (StateT (Maybe (Handle, Int)) IO) Word8
    & Stream.chunksOf2 (180 * 1024 * 1024) newHandle Handle.write2 -- SerialT (StateT (Maybe (Handle, Int)) IO) ()
    & Stream.evalStateT (return Nothing)  -- SerialT IO ()
    & Stream.drain                        -- SerialT IO ()

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode
    splitFile src
