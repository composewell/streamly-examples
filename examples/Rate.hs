import Data.Function ((&))
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Stream
import qualified Streamly.Internal.Data.Stream as Stream (timestamped)

main :: IO ()
main =
      Stream.sequence (Stream.repeat (pure "tick"))
    & Stream.timestamped
    & Stream.parEval (Stream.avgRate 1)
    & Stream.fold (Fold.drainMapM print)
