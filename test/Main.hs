{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Safe

import Control.Monad.StateVar

import Control.Concurrent.STM
import Data.IORef
import Data.STRef
import Control.Exception
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as L

-- Functional dependency needed so tryErrorCall's callback can refer to an
-- outside var.
class (HasGet m v, HasPut m v, Monad m) => TestStateVar m v | m -> v where
    tryErrorCall :: Maybe (m a -> m (Either ErrorCall a))

instance TestStateVar IO IORef where
    tryErrorCall = Just try

instance TestStateVar STM TVar where
    tryErrorCall = Just trySTM

instance TestStateVar (ST s) (STRef s) where
    tryErrorCall = Nothing

instance TestStateVar (L.ST s) (STRef s) where
    tryErrorCall = Nothing

trySTM :: Exception e => STM a -> STM (Either e a)
trySTM a = catchSTM (a >>= \v -> return (Right v)) (\e -> return (Left e))

testStateVar :: TestStateVar m v => v Int -> m ()
testStateVar var = do
    0 <- get var
    put var 1
    1 <- get var
    var $= 2
    2 <- get var

    modify var (+1)
    3 <- get var
    modify' var (+1)
    4 <- get var
    var $~ (+1)
    5 <- get var
    var $~! (+1)
    6 <- get var
    6 <- swap var 7
    7 <- get var

    case tryErrorCall of
        Nothing -> return ()
        Just tryE -> do
            Left (ErrorCall _) <- tryE $ do
                put' var undefined
            7 <- get var

            -- These tests assume the var may contain 'undefined'.
            modify var (\_ -> undefined)
            modify var undefined
            modify var (+1)
            _ <- get var
            Left (ErrorCall _) <- tryE $ do
                modify' var (+1)
            return ()

-- testIORef and testSTRef just make sure casual usage type check without
-- confusing ambiguities.

testIORef :: IO ()
testIORef = do
    var <- new 5 :: IO (IORef Int)
    5 <- get var
    put var 6
    6 <- get var
    modify var (+3)
    9 <- get var
    return ()

testSTRef :: IO ()
testSTRef = do
    9 <- return $ runST $ do
        var <- new 5 :: ST s (STRef s Int)
        5 <- get var
        put var 6
        6 <- get var
        modify var (+3)
        get var
    return ()

main :: IO ()
main = do
    testSafe
    new 0 >>= testStateVar
    atomically $ new 0 >>= testStateVar
    () <- return $ runST $ new 0 >>= testStateVar
    () <- return $ L.runST $ new 0 >>= testStateVar
    testIORef
    testSTRef

    putStrLn "Passed."
