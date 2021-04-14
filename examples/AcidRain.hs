{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

-- Copyright   : (c) 2017 Harendra Kumar
--               (c) 2013, 2014 Gabriel Gonzalez
--
-- This example is adapted from Gabriel Gonzalez's pipes-concurrency package.
-- https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState, get, modify, runStateT)
import Streamly.Prelude (MonadAsync, SerialT)

import qualified Streamly.Prelude as Stream

data Event = Quit | Harm Int | Heal Int deriving (Show)

userAction :: MonadAsync m => SerialT m Event
userAction = Stream.repeatM $ liftIO askUser
    where
    askUser = do
        command <- getLine
        case command of
            "potion" -> return (Heal 10)
            "harm"   -> return (Harm 10)
            "quit"   -> return Quit
            _        -> putStrLn "Type potion or harm or quit" >> askUser

acidRain :: MonadAsync m => SerialT m Event
acidRain = Stream.asyncly
    $ Stream.constRate 1
    $ Stream.repeatM
    $ liftIO $ return $ Harm 1

data Result = Check | Done

runEvents :: (MonadAsync m, MonadState Int m) => SerialT m Result
runEvents = do
    event <- userAction `Stream.parallel` acidRain
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
            liftIO $ if (h <= 0)
                     then putStrLn "You die!" >> return GameOver
                     else putStrLn ("Health = " <> show h) >> return Alive

main :: IO ()
main = do
    putStrLn "Your health is deteriorating due to acid rain,\\
             \ type \"potion\" or \"quit\""
    let runGame = Stream.drainWhile (== Alive)
                $ Stream.mapM getStatus runEvents
    void $ runStateT runGame 60
