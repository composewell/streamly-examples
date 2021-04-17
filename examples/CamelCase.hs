-- ghc -O2  -fspec-constr-recursive=10 -fmax-worker-args=16
-- Convert the input file to camel case and write to stdout

import Data.Function ((&))
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), openFile, stdout)

import qualified Streamly.Prelude as Stream
import qualified Streamly.FileSystem.Handle as Handle

-- | @camelCase source-file dest-file@
camelCase :: Handle -> Handle -> IO ()
camelCase src dst =
      Stream.unfold Handle.read src      -- SerialT IO Word8
    & Stream.scanl' step (True, Nothing) -- SerialT IO (Bool, Maybe Word8)
    & Stream.mapMaybe snd                -- SerialT IO Word8
    & Stream.fold (Handle.write dst)     -- IO ()

    where

    isNewline x = x == 0x0a
    isUpper x = x >= 0x41 && x <= 0x5a
    isLower x = x >= 0x61 && x <= 0x7a

    -- Scan accumulator @(wasSpace, output)@ contains whether the previous
    -- character was white space, and if the current char should be emitted in
    -- the output.
    step :: (Bool, Maybe Word8) -> Word8 -> (Bool, Maybe Word8)
    step (wasSpace, _) x
        -- Newline or upper case chars go unmodified
        | isNewline x || isUpper x = (False, Just x)
        -- Convert lower to upper if preceded by whitespace
        | isLower x = (False, Just $ if wasSpace then x - 32 else x)
        | otherwise = (True, Nothing)

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode
    camelCase src stdout
