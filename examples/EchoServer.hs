-- A concurrent TCP server that echoes everything that it receives.

import Control.Monad.Catch (finally)
import Data.Function ((&))
import Network.Socket (Socket)

import qualified Network.Socket as Net
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Prelude as Stream

main :: IO ()
main =
      Stream.unfold TCP.acceptOnPort 8091 -- ParallelT IO Socket
    & Stream.mapM (handleWithM echo)      -- ParallelT IO ()
    & Stream.fromParallel                 -- SerialT IO ()
    & Stream.drain                        -- IO ()

    where

    echo :: Socket -> IO ()
    echo sk =
          Stream.unfold Socket.readChunksWithBufferOf (32768, sk) -- SerialT IO (Array Word8)
        & Stream.fold (Socket.writeChunks sk)                     -- IO ()

    handleWithM :: (Socket -> IO ()) -> Socket -> IO ()
    handleWithM f sk = finally (f sk) (Net.close sk)
