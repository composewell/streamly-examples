import Data.Function ((&))
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as Stream (timestamped)
import qualified Streamly.Data.Stream.Concurrent  as Concur

main :: IO ()
main =
      Stream.sequence (Stream.repeat (pure "tick"))
    & Stream.timestamped
    & Concur.parEval (Concur.avgRate 1)
    & Stream.fold (Fold.drainMapM print)
