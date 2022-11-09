{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wmissing-methods #-}

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

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
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

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as Stream (crossApplySnd)

------------------------------------------------------------------------------
-- Applicative/Monad instances
------------------------------------------------------------------------------

instance Monad m => Applicative (Stream m) where

    {-# INLINE pure #-}
    pure = Stream.fromPure

    {-# INLINE (*>) #-}
    s1 *> s2 = Stream.crossApplySnd s1 s2

instance Monad m => Monad (Stream m) where
    return = pure

    {-# INLINE (>>) #-}
    (>>) = (*>)

instance MonadThrow m => MonadThrow (Stream m) where
  throwM = lift . throwM

instance MonadTrans Stream where
    {-# INLINE lift #-}
    lift = Stream.fromEffect

instance (MonadIO m) => MonadIO (Stream m) where
    liftIO x = Stream.fromEffect $ liftIO x

-------------------------------------------------------------------------------
-- Using MaybeT below streamly
-------------------------------------------------------------------------------
--
-- When streamly is on top MaybeT would terminate all iterations of
-- non-determinism.
--
getSequenceMaybeBelow
    :: ( Monad m
       , MonadIO (Stream (MaybeT m))
       )
    => Stream (MaybeT m) ()
getSequenceMaybeBelow = do
    liftIO $ putStrLn "MaybeT below streamly: Enter one char per line: "

    i <- Stream.fromFoldable [1..2 :: Int]
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
--
-- When MaybeT is on top a Nothing would terminate only the current iteration
-- of non-determinism below.
--
-- Note that this is redundant configuration as the same behavior can be
-- achieved with just streamly, using mzero.
--
getSequenceMaybeAbove :: ( MonadIO (Stream m)) => MaybeT (Stream m) ()
getSequenceMaybeAbove = do
    liftIO $ putStrLn "MaybeT above streamly: Enter one char per line: "

    i <- lift $ Stream.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") mzero

    r2 <- liftIO getLine
    when (r2 /= "y") mzero

mainMaybeAbove :: ( MonadIO (Stream m)) => MaybeT (Stream m) ()
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
getSequenceEitherBelow
    :: (  Monad m
       , MonadIO (Stream (ExceptT String m))
       )
    => Stream (ExceptT String m) ()
getSequenceEitherBelow = do
    liftIO $ putStrLn "ExceptT below streamly: Enter one char per line: "

    i <- Stream.fromFoldable [1..2 :: Int]
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
--
-- XXX does not work correctly yet
--
getSequenceEitherAsyncBelow
    :: (  MonadIO m
       , MonadIO (Stream (ExceptT String m))
       )
    => Stream (ExceptT String m) ()
getSequenceEitherAsyncBelow = do
    liftIO $ putStrLn "ExceptT below concurrent streamly: "

    i <- (liftIO (threadDelay 1000)
            >> lift (throwE "First task")
            >> return 1)
            <> (lift (throwE "Second task") >> return 2)
            <> Stream.fromPure (3 :: Integer)
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
getSequenceEitherAbove :: ( MonadIO (Stream m))
    => ExceptT String (Stream m) ()
getSequenceEitherAbove = do
    liftIO $ putStrLn "ExceptT above streamly: Enter one char per line: "

    i <- lift $ Stream.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") $ throwE $ "Expecting x got: " <> r1

    r2 <- liftIO getLine
    when (r2 /= "y") $ throwE $ "Expecting y got: " <> r2

mainEitherAbove :: ( MonadIO (Stream m)) => ExceptT String (Stream m) ()
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
getSequenceMonadThrow :: ( MonadIO (Stream m), MonadThrow (Stream m))
    => Stream m ()
getSequenceMonadThrow = do
    liftIO $ putStrLn "MonadThrow in streamly: Enter one char per line: "

    i <- Stream.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") $ throwM $ Unexpected $ "Expecting x got: " <> r1

    r2 <- liftIO getLine
    when (r2 /= "y") $ throwM $ Unexpected $ "Expecting y got: " <> r2

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
getSequenceContBelow
    :: ( MonadIO m, MonadIO (Stream (ContT r m)))
    => Stream (ContT r m) (Either String ())
getSequenceContBelow = do
    liftIO $ putStrLn "ContT below streamly: Enter one char per line: "

    i <- Stream.fromFoldable [1..2 :: Int]
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

mainContBelow
    :: ( MonadIO m, MonadIO (Stream (ContT r m)))
    => Stream (ContT r m) ()
mainContBelow = do
    r <- getSequenceContBelow
    case r of
        Right _ -> liftIO $ putStrLn "Bingo"
        Left s  -> liftIO $ putStrLn s

-------------------------------------------------------------------------------
-- Using ContT above streamly
-------------------------------------------------------------------------------
--
getSequenceContAbove :: ( MonadIO (Stream m))
    => ContT r (Stream m) (Either String ())
getSequenceContAbove = do
    liftIO $ putStrLn "ContT above streamly: Enter one char per line: "

    i <- lift $ Stream.fromFoldable [1..2 :: Int]
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

mainContAbove :: ( MonadIO (Stream m)) => ContT r (Stream m) ()
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
    Stream.fold Fold.drain $ runMaybeT mainMaybeAbove
    runContT (Stream.fold Fold.drain mainContBelow) return
    Stream.fold Fold.drain (runContT mainContAbove return)
    mainEitherBelow
    Stream.fold Fold.drain (runExceptT mainEitherAbove)
    mainMonadThrow
    mainEitherAsyncBelow
