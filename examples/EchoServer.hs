-- A concurrent TCP server that echoes everything that it receives.

import Control.Monad.Catch (finally)
import Data.Function ((&))
import Network.Socket (Socket)

import qualified Network.Socket as Net
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket

main :: IO ()
main =
      Stream.unfold TCP.acceptorOnPort 8091 -- Stream IO Socket
    & Concur.mapM (handleWithM echo)        -- Stream IO ()
    & Stream.fold Fold.drain                -- IO ()

    where

    echo :: Socket -> IO ()
    echo sk =
          Stream.unfold Socket.chunkReaderWith (32768, sk) -- Stream IO (Array Word8)
        & Stream.fold (Socket.writeChunks sk)              -- IO ()

    handleWithM :: (Socket -> IO ()) -> Socket -> IO ()
    handleWithM f sk = finally (f sk) (Net.close sk)
