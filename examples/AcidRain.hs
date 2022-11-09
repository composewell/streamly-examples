{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

-- Copyright   : (c) 2017 Harendra Kumar
--               (c) 2013, 2014 Gabriel Gonzalez
--
-- This example is adapted from Gabriel Gonzalez's pipes-concurrency package.
-- https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function ((&))
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Concurrent as Concur

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Bottom as Stream (takeEndBy)
import qualified Streamly.Internal.Data.Stream.Concurrent as Concur (parEager)

data Event = Quit | Harm Int | Heal Int deriving (Eq, Show)

userAction :: MonadAsync m => Stream m Event
userAction =  Stream.repeatM $ liftIO askUser
    where
    askUser = do
        command <- getLine
        case command of
            "potion" -> return (Heal 10)
            "harm"   -> return (Harm 10)
            "quit"   -> return Quit
            _        -> putStrLn "Type potion or harm or quit" >> askUser

acidRain :: MonadAsync m => Stream m Event
acidRain =
    Concur.parRepeatM (Concur.constRate 1) (return $ Harm 1)

data Result = Check | Done

runEvents :: MonadAsync m => IORef Int -> Stream m Result
runEvents ref =
    let events = Concur.parEager [userAction, acidRain]
        e2 = Stream.takeEndBy (== Quit ) events     -- Stream m Event
      in Stream.mapM modCount e2                    -- Stream m Result

    where

    modCount ev =
        case  ev of
            Harm n -> liftIO $ modifyIORef ref (\h -> h - n) >> return Check
            Heal n -> liftIO $ modifyIORef ref (+ n) >> return Check
            Quit -> return Done

data Status = Alive | GameOver deriving Eq

getStatus :: MonadAsync m => IORef Int -> Result -> m Status
getStatus ref result =
    liftIO $
        case result of
            Done  -> putStrLn "You quit!" >> return GameOver
            Check -> do
                h <- readIORef ref
                if h <= 0
                        then putStrLn "You die!" >> return GameOver
                        else putStrLn ("Health = " <> show h) >> return Alive

main :: IO ()
main = do
    putStrLn "Your health is deteriorating due to acid rain,\\
        \ type \"potion\" or \"quit\""
    ref <- newIORef 60
    let runGame =
          Stream.mapM (getStatus ref) (runEvents ref)
            & Stream.fold (Fold.takeEndBy (/= Alive) Fold.drain)
    void $ runGame
