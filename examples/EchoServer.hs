-- A concurrent TCP server that echoes everything that it receives.

import Control.Monad.Catch (finally, MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Network.Socket (Socket)
import qualified Network.Socket as Net

import Streamly.Network.Socket

import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as Stream

main :: IO ()
main =
      Stream.serially (Stream.unfold TCP.acceptOnPort 8091)
    & Stream.parallely . Stream.mapM (handleWithM echo)
    & Stream.drain

    where

    echo sk =
          Stream.unfold readChunksWithBufferOf (32768, sk) -- SerialT IO Socket
        & Stream.fold (writeChunks sk)                     -- IO ()

handleWithM :: (MonadMask m, MonadIO m) => (Socket -> m ()) -> Socket -> m ()
handleWithM f sk = finally (f sk) (liftIO (Net.close sk))
