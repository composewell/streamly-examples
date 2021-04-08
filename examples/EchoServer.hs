-- A concurrent TCP server that echoes everything that it receives.

import Control.Monad.Catch (finally, MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Network.Socket (Socket)
import qualified Network.Socket as Net

import Streamly.Network.Socket

import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as S

main :: IO ()
main =
      S.serially (S.unfold TCP.acceptOnPort 8091)
    & S.parallely . S.mapM (handleWithM echo)
    & S.drain

    where

    echo sk =
          S.unfold readChunksWithBufferOf (32768, sk) -- SerialT IO Socket
        & S.fold (writeChunks sk)                     -- IO ()

handleWithM :: (MonadMask m, MonadIO m) => (Socket -> m ()) -> Socket -> m ()
handleWithM f sk = finally (f sk) (liftIO (Net.close sk))
