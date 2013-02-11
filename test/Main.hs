{-# LANGUAGE CPP #-}
import Safe

import Control.Monad.StateVar

import Control.Concurrent.STM
import Data.IORef
import Data.STRef
import Control.Monad.ST

main :: IO ()
main = testSafe
