-- A concurrent TCP server that:
--
-- * receives connections from clients
-- * splits the incoming data into lines
-- * lines from concurrent connections are merged into a single srteam
-- * writes the line stream to an output file

import Control.Monad.IO.Class (liftIO)
import Network.Socket (close)
import System.Environment (getArgs)

import Streamly.Unicode.Stream
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.Network.Socket as NS
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as Stream

import System.IO (withFile, IOMode(..))

main :: IO ()
main = do
    file <- fmap head getArgs
    withFile file AppendMode
        (\src -> Stream.fold (FH.write src)
        $ encodeLatin1
        $ Stream.unfoldMany Array.read
        $ Stream.concatMapWith Stream.parallel use
        $ Stream.unfold TCP.acceptOnPort 8090)

    where

    use sk = Stream.finally (liftIO $ close sk) (recv sk)
    recv =
          Stream.splitWithSuffix (== '\n') Array.write
        . decodeLatin1
        . Stream.unfold NS.read
