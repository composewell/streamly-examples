{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

-- Copyright   : (c) 2017 Harendra Kumar
--               (c) 2013, 2014 Gabriel Gonzalez
--
-- This example is adapted from Gabriel Gonzalez's pipes-concurrency package.
-- https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState, get, modify)
import Data.Function ((&))
import Streamly.Data.Stream.Prelude (MonadAsync, Stream)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream

data Event = Quit | Harm Int | Heal Int deriving (Eq, Show)

userAction :: MonadAsync m => Stream m Event
userAction = Stream.repeatM $ liftIO askUser
    where
    askUser = do
        command <- getLine
        case command of
            "potion" -> return (Heal 10)
            "harm"   -> return (Harm 10)
            "quit"   -> return Quit
            _        -> putStrLn "Type potion or harm or quit" >> askUser

acidRain :: MonadAsync m => Stream m Event
acidRain = Stream.parRepeatM (Stream.constRate 1) (return $ Harm 1)

parallel :: MonadAsync m => [Stream m a] -> Stream m a
parallel = Stream.parList (Stream.eager True)

data Result = Check | Done

runEvents :: (MonadAsync m, MonadState Int m) => Stream m Result
runEvents =
    Stream.mapM f $ parallel [userAction, acidRain]

    where

    f event =
        case event of
            Harm n -> modify (\h -> h - n) >> return Check
            Heal n -> modify (\h -> h + n) >> return Check
            Quit -> return Done

data Status = Alive | GameOver deriving Eq

getStatus :: (MonadAsync m, MonadState Int m) => Result -> m Status
getStatus result =
    case result of
        Done  -> liftIO $ putStrLn "You quit!" >> return GameOver
        Check -> do
            h <- get
            liftIO
                $ if (h <= 0)
                  then putStrLn "You die!" >> return GameOver
                  else putStrLn ("Health = " <> show h) >> return Alive

main :: IO ()
main = do
    putStrLn "Your health is deteriorating due to acid rain,\\
             \ type \"potion\" or \"quit\""
    Stream.mapM getStatus runEvents  -- Stream (StateT Int IO) Status
        & Stream.runStateT (pure 60) -- Stream IO (Int, Status)
        & fmap snd                   -- Stream IO Status
        & Stream.fold (Fold.takeEndBy (/= Alive) Fold.drain) -- IO ()
    return ()
