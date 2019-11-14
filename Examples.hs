import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Data.Functor.Identity
import Data.Function ((&))
import Data.Word
import Data.Char
import Data.Map.Strict hiding (map, lookup)
import Network.HTTP.Simple
import Network.Socket

import Streamly (SerialT, asyncly, aheadly)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A
import qualified Streamly.Network.Socket as SK
import qualified Streamly.Network.Inet.TCP as TCP

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Unicode.Stream as U
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Network.Socket as SK

sumInt :: Identity Int
sumInt =
      S.unfold UF.fromList [1..10] -- SerialT Identity Int
    & S.fold   FL.sum              -- Identity Int

sumInt1 :: Identity Int
sumInt1 =
      S.fromList [1..10]       -- SerialT Identity Int
    & S.sum                    -- Identity Int

echo :: IO ()
echo =
      FH.getChunks            -- SerialT IO (Array Word8)
    & FH.putChunks            -- IO ()

cat :: IO ()
cat =
      File.toChunks "inFile"    -- SerialT IO (Array Word8)
    & FH.putChunks              -- IO ()

cp :: IO ()
cp =
      File.toChunks "inFile"    -- SerialT IO (Array Word8)
    & File.fromChunks "outFile" -- IO ()

wcc :: IO Int
wcc =
      File.toBytes "inFile" -- SerialT IO Word8
    & S.fold FL.length      -- IO Int

countl :: Int -> Word8 -> Int
countl n ch = if (ch == 10) then n + 1 else n

nlines :: Monad m => Fold m Word8 Int
nlines = FL.mkPure countl 0 id

wcl :: IO Int
wcl =
      File.toBytes "inFile" -- SerialT IO Word8
    & S.fold nlines         -- IO Int

countw :: (Int, Bool) -> Word8 -> (Int, Bool)
countw (n, wasSpace) ch =
        if (isSpace $ chr $ fromIntegral ch)
        then (n, True)
        else (if wasSpace then n + 1 else n, False)

nwords :: Monad m => Fold m Word8 Int
nwords = FL.mkPure countw (0, True) fst

wcw :: IO Int
wcw =
      File.toBytes "inFile"   -- SerialT IO Word8
    & S.fold nwords           -- IO Int

wc :: IO (Int, Int, Int)
wc =
      File.toBytes "inFile"   -- SerialT IO Word8
    & S.fold ((,,) <$> nlines <*> nwords <*> FL.length)
                              -- IO (Int, Int, Int)

tee :: IO ((),())
tee =
      File.toBytes "inFile"        -- SerialT IO Word8
    & S.fold (FL.tee (File.write "outFile1") (File.write "outFile2"))
                               -- IO ((),())
bucket :: Int -> (Int, Int)
bucket n = let i = n `mod` 10
           in if i > 9 then (9,n) else (i,n)

wordHisto :: IO (Map Int Int)
wordHisto =
      File.toBytes "inFile"            -- SerialT IO Word8
    & U.decodeLatin1                   -- SerialT IO Char
    & U.words FL.length                -- SerialT IO Int
    & S.map bucket                     -- SerialT IO (Int, Int)
    & S.trace print
    & S.fold (FL.classify FL.length)   -- IO (Map (Int, Int))

appendAll :: IO ()
appendAll =
      Dir.toFiles "appendAll"        -- SerialT IO String
    & S.concatUnfold File.read       -- SerialT IO Word8
    & File.fromBytes "outFile"       -- IO()

mult :: (Int, Int) -> Int
mult (x, y) = x * y

from :: Monad m => Unfold m Int Int
from = UF.enumerateFromToIntegral 1000

cross :: Monad m => Unfold m (Int, Int) Int
cross =
      UF.outerProduct from from
    & UF.map mult

loops :: SerialT IO ()
loops = do
    x <- S.fromList [1,2]
    y <- S.fromList [3,4]
    S.yieldM $ putStrLn $ show (x, y)

get :: String -> IO String
get s = liftIO (httpNoBody (parseRequest_ s)) >> return s

fetch :: String -> IO (String, String)
fetch w =
    (,) <$> pure w <*> get ( "https://www.google.com/search?q=" ++ w)

wordList :: [String]
wordList = ["cat", "dog", "mouse"]

meanings :: [IO (String, String)]
meanings = map fetch wordList

getWords :: IO ()
getWords =
      S.fromListM meanings                  -- SerialT  IO (String, String)
    & aheadly
    & S.map show                            -- SerialT IO String
    & S.intercalateSuffix "\n" UF.identity  -- SerialT IO String
    & S.map A.fromList                      -- SerialT IO (Array Word8)
    & FH.putChunks                          -- IO ()

lookupWords :: Socket -> IO ()
lookupWords sk =
      S.unfold SK.read sk                   -- SerialT IO Word8
    & U.decodeLatin1                        -- SerialT IO Char
    & U.words FL.toList                     -- SerialT IO String
    & S.serially                            -- AheadT  IO String
    & S.mapM fetch                          -- AheadT  IO (String, String)
    & S.aheadly                             -- SerialT IO (String, String)
    & S.map show                            -- SerialT IO String
    & S.intercalateSuffix "\n" UF.identity  -- SerialT IO String
    & S.fold (SK.writeStrings sk)           -- IO ()

serve :: Socket -> IO ()
serve sk = finally (lookupWords sk) (close sk)

wordserver :: IO ()
wordserver =
      S.unfold TCP.acceptOnPort 8091 -- SerialT IO Socket
    & S.serially                     -- AsyncT IO ()
    & S.mapM serve                   -- AsyncT IO ()
    & S.asyncly                      -- SerialT IO ()
    & S.drain                        -- IO ()

readWords :: Socket -> SerialT IO String
readWords sk =
    S.unfold SK.read sk -- SerialT IO Word8
  & U.decodeLatin1      -- SerialT IO Char
  & U.words FL.toList   -- SerialT IO String

recv :: Socket -> SerialT IO String
recv sk = S.finally (liftIO $ close sk) (readWords sk)

mergeStreams :: IO ()
mergeStreams =
      S.unfold TCP.acceptOnPort 8091   -- SerialT IO Socket
    & S.concatMapWith S.parallel recv  -- SerialT IO String
    & U.unwords UF.fromList            -- SerialT IO Char
    & U.encodeLatin1                   -- SerialT IO Word8
    & File.fromBytes "outFile"         -- IO ()

concurrentFolds :: IO ()
concurrentFolds =
      FH.getBytes                            -- SerialT IO Word8
    & S.tapAsync (File.fromBytes "outFile1") -- SerialT IO Word8
    & S.tapAsync (File.fromBytes "outFile2") -- SerialT IO Word8
    & File.fromBytes "outFile"               -- IO ()

main :: IO ()
main = do
    -- print $ runIdentity sumInt
    -- print $ runIdentity sumInt1
    -- echo
    -- cat
    -- cp
    -- wcc >>= print
    -- wcl >>= print
    -- wcw >>= print
    -- wc >>= print
    -- void tee
    -- wordHisto >>= print
    -- appendAll
    -- UF.fold cross FL.sum (1,1) >>= print
    -- S.drain loops
    -- getWords
    -- wordserver
    -- mergeStreams
    concurrentFolds
