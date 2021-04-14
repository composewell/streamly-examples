-- ghc -O2  -fspec-constr-recursive=10 -fmax-worker-args=16
-- Convert the input file to camel case and write to stdout

import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), openFile, stdout)

import qualified Streamly.Prelude as Stream
import qualified Streamly.FileSystem.Handle as Handle

camelCase :: Handle -> Handle -> IO ()
camelCase src dst =
      Stream.fold (Handle.write dst)
    $ Stream.map fromJust
    $ Stream.filter isJust
    $ Stream.map snd
    $ Stream.scanl' step (True, Nothing)
    $ Stream.unfold Handle.read src

    where

    step (wasSpace, _) x =
        if x == 0x0a || x >= 0x41 && x <= 0x5a
        then (False, Just x)
        else if x >= 0x61 && x <= 0x7a
             then (False, Just $ if wasSpace then x - 32 else x)
             else (True, Nothing)

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode
    camelCase src stdout
