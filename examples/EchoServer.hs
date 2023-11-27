-- A concurrent TCP server that echoes everything that it receives.

import Control.Monad.Catch (finally)
import Data.Function ((&))
import Network.Socket (Socket)

import qualified Network.Socket as Net
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket

main :: IO ()
main =
      TCP.accept 8091                       -- Stream IO Socket
    & Stream.parMapM id (handleWithM echo)  -- Stream IO ()
    & Stream.fold Fold.drain                -- IO ()

    where

    echo :: Socket -> IO ()
    echo sk =
          Socket.readChunksWith 32768 sk      -- Stream IO (Array Word8)
        & Stream.fold (Socket.writeChunks sk) -- IO ()

    handleWithM :: (Socket -> IO ()) -> Socket -> IO ()
    handleWithM f sk = finally (f sk) (Net.close sk)
