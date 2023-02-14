{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-------------------------------------------------------------------------------
-- Combining control flow manipulating monad transformers (MaybeT, exceptT,
-- ContT) with Streamly
-------------------------------------------------------------------------------
--
-- Streamly streams are non-determinism (nested looping) monads. We can use a
-- control flow monad on top or streamly on top depending on whether we want to
-- superimpose control flow manipulation on top of non-deterministic
-- composition or vice-versa.
--
-- This file provides an example where we enter a sequence of characters "x",
-- and "y" on separate lines, on the command line. When any other sequence is
-- entered the control flow short circuits at the first non-matching char and
-- exits.

import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad (when, mzero)
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, catchE)
import Control.Monad.Trans.Cont (ContT(..), callCC)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Stream.StreamD (CrossStream, mkCross, unCross)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.StreamK as K

-------------------------------------------------------------------------------
-- Using MaybeT below streamly
-------------------------------------------------------------------------------
--
-- When streamly is on top MaybeT would terminate all iterations of
-- non-determinism.
--
getSequenceMaybeBelow :: MonadIO m => Stream (MaybeT m) ()
getSequenceMaybeBelow = unCross $ do
    liftIO $ putStrLn "MaybeT below streamly: Enter one char per line: "

    i <- mkCross $ K.toStream $ K.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") $ lift mzero

    r2 <- liftIO getLine
    when (r2 /= "y") $ lift mzero

mainMaybeBelow :: IO ()
mainMaybeBelow = do
    r <- runMaybeT (Stream.fold Fold.drain getSequenceMaybeBelow)
    case r of
        Just _ -> putStrLn "Bingo"
        Nothing -> putStrLn "Wrong"

-------------------------------------------------------------------------------
-- Using MaybeT above streamly
-------------------------------------------------------------------------------

-- When MaybeT is on top a Nothing would terminate only the current iteration
-- of non-determinism below.
--
-- Note that this is redundant configuration as the same behavior can be
-- achieved with just streamly, using mzero.
--
getSequenceMaybeAbove :: MonadIO m => MaybeT (CrossStream m) ()
getSequenceMaybeAbove = do
    liftIO $ putStrLn "MaybeT above streamly: Enter one char per line: "

    i <- lift $ mkCross $ K.toStream $ K.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") mzero

    r2 <- liftIO getLine
    when (r2 /= "y") mzero

mainMaybeAbove :: MonadIO m => MaybeT (CrossStream m) ()
mainMaybeAbove = do
    getSequenceMaybeAbove
    liftIO $ putStrLn "Bingo"

-------------------------------------------------------------------------------
-- Using ExceptT below streamly
-------------------------------------------------------------------------------
--
-- XXX need to have a specialized liftCatch to lift catchE
--
-- Note that throwE would terminate all iterations of non-determinism
-- altogether.
getSequenceEitherBelow :: MonadIO m => Stream (ExceptT String m) ()
getSequenceEitherBelow = unCross $ do
    liftIO $ putStrLn "ExceptT below streamly: Enter one char per line: "

    i <- mkCross $ K.toStream $ K.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") $ lift $ throwE $ "Expecting x got: " <> r1

    r2 <- liftIO getLine
    when (r2 /= "y") $ lift $ throwE $ "Expecting y got: " <> r2

mainEitherBelow :: IO ()
mainEitherBelow = do
    -- XXX Cannot lift catchE
    r <- runExceptT (Stream.fold Fold.drain getSequenceEitherBelow)
    case r of
        Right _ -> liftIO $ putStrLn "Bingo"
        Left s  -> liftIO $ putStrLn s

-------------------------------------------------------------------------------
-- Using ExceptT below concurrent streamly
-------------------------------------------------------------------------------

-- XXX does not work correctly yet
--

getSequenceEitherAsyncBelow :: MonadIO m => Stream (ExceptT String m) ()
getSequenceEitherAsyncBelow = unCross $ do
    liftIO $ putStrLn "ExceptT below concurrent streamly: "

    i <- mkCross
            $ Stream.consM
                (liftIO (threadDelay 1000)
                    >> throwE "First task"
                    >> return 1
                )
                (Stream.consM
                    (throwE "Second task" >> return 2)
                    (Stream.fromPure (3 :: Integer))
                )
    liftIO $ putStrLn $ "iteration = " <> show i

mainEitherAsyncBelow :: IO ()
mainEitherAsyncBelow = do
    r <- runExceptT (Stream.fold Fold.drain getSequenceEitherAsyncBelow)
    case r of
        Right _ -> liftIO $ putStrLn "Bingo"
        Left s  -> liftIO $ putStrLn s

-------------------------------------------------------------------------------
-- Using ExceptT above streamly
-------------------------------------------------------------------------------
--
-- When ExceptT is on top, we can lift the non-determinism of stream from
-- below.
--
-- Note that throwE would terminate/break only current iteration of
-- non-determinism and not all of them altogether.
--
-- Here we can use catchE directly but will have to use monad-control to lift
-- stream operations with stream arguments.
getSequenceEitherAbove :: MonadIO m => ExceptT String (CrossStream m) ()
getSequenceEitherAbove = do
    liftIO $ putStrLn "ExceptT above streamly: Enter one char per line: "

    i <- lift $ mkCross $ K.toStream $ K.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") $ throwE $ "Expecting x got: " <> r1

    r2 <- liftIO getLine
    when (r2 /= "y") $ throwE $ "Expecting y got: " <> r2

mainEitherAbove :: MonadIO m => ExceptT String (CrossStream m) ()
mainEitherAbove =
    catchE (getSequenceEitherAbove >> liftIO (putStrLn "Bingo"))
           (liftIO . putStrLn)

-------------------------------------------------------------------------------
-- Using MonadThrow to throw exceptions in streamly
-------------------------------------------------------------------------------
--
newtype Unexpected = Unexpected String deriving Show

instance Exception Unexpected

-- Note that unlike when ExceptT is used on top, MonadThrow terminates all
-- iterations of non-determinism rather then just the current iteration.
--
getSequenceMonadThrow :: (MonadIO m, MonadThrow m) => Stream m ()
getSequenceMonadThrow = unCross $ do
    liftIO $ putStrLn "MonadThrow in streamly: Enter one char per line: "

    i <- mkCross $ K.toStream $ K.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x")
        $ mkCross
        $ Stream.fromEffect $ throwM $ Unexpected $ "Expecting x got: " <> r1

    r2 <- liftIO getLine
    when (r2 /= "y")
        $ mkCross
        $ Stream.fromEffect $ throwM $ Unexpected $ "Expecting y got: " <> r2

mainMonadThrow :: IO ()
mainMonadThrow =
    catch (Stream.fold Fold.drain getSequenceMonadThrow >> liftIO (putStrLn "Bingo"))
          (\(e :: SomeException) -> liftIO $ print e)

-------------------------------------------------------------------------------
-- Using ContT below streamly
-------------------------------------------------------------------------------
--
-- CallCC is the goto/setjmp/longjmp equivalent
-- Allows us to manipulate the control flow in arbitrary ways
--
-- XXX need to have a specialized liftCallCC to actually lift callCC
--
getSequenceContBelow :: MonadIO m => CrossStream (ContT r m) (Either String ())
getSequenceContBelow = do
    liftIO $ putStrLn "ContT below streamly: Enter one char per line: "

    i <- mkCross $ K.toStream $ K.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r <- lift $ callCC $ \exit -> do
        r1 <- liftIO getLine
        _ <- if r1 /= "x"
             then exit $ Left $ "Expecting x got: " <> r1
             else return $ Right ()

        r2 <- liftIO getLine
        if r2 /= "y"
        then exit $ Left $ "Expecting y got: " <> r2
        else return $ Right ()
    liftIO $ putStrLn $ "done iteration = " <> show i
    return r

mainContBelow :: MonadIO m => Stream (ContT r m) ()
mainContBelow = unCross $ do
    r <- getSequenceContBelow
    case r of
        Right _ -> liftIO $ putStrLn "Bingo"
        Left s  -> liftIO $ putStrLn s

-------------------------------------------------------------------------------
-- Using ContT above streamly
-------------------------------------------------------------------------------
--
getSequenceContAbove :: MonadIO m => ContT r (CrossStream m) (Either String ())
getSequenceContAbove = do
    liftIO $ putStrLn "ContT above streamly: Enter one char per line: "

    i <- lift $ mkCross $ K.toStream $ K.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    callCC $ \exit -> do
        r1 <- liftIO getLine
        _ <- if r1 /= "x"
             then exit $ Left $ "Expecting x got: " <> r1
             else return $ Right ()

        r2 <- liftIO getLine
        if r2 /= "y"
        then exit $ Left $ "Expecting y got: " <> r2
        else return $ Right ()

mainContAbove :: MonadIO m => ContT r (CrossStream m) ()
mainContAbove = do
    r <- getSequenceContAbove
    case r of
        Right _ -> liftIO $ putStrLn "Bingo"
        Left s  -> liftIO $ putStrLn s

-------------------------------------------------------------------------------
-- Combining control flow manipulating monad transformers (MaybeT, exceptT,
-- ContT) with Streamly
-------------------------------------------------------------------------------

main :: IO ()
main = do
    mainMaybeBelow
    Stream.fold Fold.drain $ unCross $ runMaybeT mainMaybeAbove
    runContT (Stream.fold Fold.drain mainContBelow) return
    Stream.fold Fold.drain $ unCross $ runContT mainContAbove return
    mainEitherBelow
    Stream.fold Fold.drain $ unCross $ runExceptT mainEitherAbove
    mainMonadThrow
    mainEitherAsyncBelow
