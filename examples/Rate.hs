import Data.Function ((&))
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Concurrent  as Async

main :: IO ()
main =
      Stream.sequence (Stream.repeat (pure "tick"))
    & Stream.timestamped
    & Async.evalWith (Async.avgRate 1)
    & Stream.fold (Fold.drainBy print)
