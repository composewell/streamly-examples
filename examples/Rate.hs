import Data.Function ((&))
import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.Data.Stream.IsStream as Stream (timestamped)

main :: IO ()
main =
      Stream.repeatM (pure "tick")  -- AsyncT IO String
    & Stream.timestamped            -- AsyncT IO (AbsTime, String)
    & Stream.avgRate 1              -- AsyncT IO (AbsTime, String)
    & Stream.fromAsync              -- SerialT IO (AbsTime, String)
    & Stream.mapM_ print            -- IO ()
