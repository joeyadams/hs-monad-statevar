{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
module Safe (
    testSafe,
) where

import Control.Monad.StateVar ()

testSafe :: IO ()
testSafe = return ()
