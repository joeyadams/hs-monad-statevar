{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Overloaded 'get' and 'put' for state variables ('IORef', 'TVar', etc.)
-- to make reading and writing more concise.
module Control.Monad.StateVar (
    -- * Overloaded get and put
    HasGet(..),
    HasPut(..),
    HasNew(..),
    put',
    new',
    modify,
    modify',
    swap,

    -- * Infix aliases
    -- All of these are @infixr 2@.
    ($=),
    ($~),
    ($=!),
    ($~!),
) where

import Control.Monad.Trans.Class
import GHC.Conc (STM, TVar, readTVar, writeTVar, newTVar)
-- import Control.Concurrent.STM
import Data.IORef

#if MIN_VERSION_base(4,4,0) && !MIN_VERSION_base(4,8,0)
import qualified Control.Monad.ST.Safe as S
import qualified Control.Monad.ST.Lazy.Safe as L
#else
import qualified Control.Monad.ST as S
import qualified Control.Monad.ST.Lazy as L
#endif
import Data.STRef (STRef)
import qualified Data.STRef as S
import qualified Data.STRef.Lazy as L

class HasGet m v where
    -- | Read the value from the variable.
    get :: v a -> m a

class HasPut m v where
    -- | Write a new value to the variable.
    put :: v a -> a -> m ()

class HasNew m v where
    -- | Create a new variable.
    new :: a -> m (v a)

-- | 'lift' $ 'get' v
instance (HasGet m v, MonadTrans t, Monad m) => HasGet (t m) v where
    get v = lift $ get v
    {-# INLINE get #-}

-- | 'lift' $ 'put' v a
instance (HasPut m v, MonadTrans t, Monad m) => HasPut (t m) v where
    put v a = lift $ put v a
    {-# INLINE put #-}

-- | 'lift' $ 'put' v a
instance (HasNew m v, MonadTrans t, Monad m) => HasNew (t m) v where
    new a = lift $ new a
    {-# INLINE new #-}

-- | 'readIORef'
instance HasGet IO IORef where
    get = readIORef
    {-# INLINE get #-}

-- | 'writeIORef'
instance HasPut IO IORef where
    put = writeIORef
    {-# INLINE put #-}

-- | 'newIORef'
instance HasNew IO IORef where
    new = newIORef
    {-# INLINE new #-}

-- | 'readTVar'
instance HasGet STM TVar where
    get = readTVar
    {-# INLINE get #-}

-- | 'writeTVar'
instance HasPut STM TVar where
    put = writeTVar
    {-# INLINE put #-}

-- | 'newTVar'
instance HasNew STM TVar where
    new = newTVar
    {-# INLINE new #-}

-- | 'S.readSTRef'
instance HasGet (S.ST s) (STRef s) where
    get = S.readSTRef
    {-# INLINE get #-}

-- | 'S.writeSTRef'
instance HasPut (S.ST s) (STRef s) where
    put = S.writeSTRef
    {-# INLINE put #-}

-- | 'S.newSTRef'
instance HasNew (S.ST s) (STRef s) where
    new = S.newSTRef
    {-# INLINE new #-}

-- | 'L.readSTRef'
instance HasGet (L.ST s) (STRef s) where
    get = L.readSTRef
    {-# INLINE get #-}

-- | 'L.writeSTRef'
instance HasPut (L.ST s) (STRef s) where
    put = L.writeSTRef
    {-# INLINE put #-}

-- | 'L.newSTRef'
instance HasNew (L.ST s) (STRef s) where
    new = L.newSTRef
    {-# INLINE new #-}

-- | Variant of 'put' that forces the value before writing it.
put' :: HasPut m v => v a -> a -> m ()
put' v x = x `seq` put v x
{-# INLINE put' #-}

-- | Variant of 'new' that forces the value before creating a variable with it.
new' :: HasNew m v => a -> m (v a)
new' x = x `seq` new x
{-# INLINE new' #-}

-- | Modify the value inside the variable with the given function.
--
-- >list <- newIORef [1,2,3]
-- >modify list (0:)  -- prepend 0 to the list
modify :: (HasGet m v, HasPut m v, Monad m) => v a -> (a -> a) -> m ()
modify v f = get v >>= put v . f
{-# INLINE modify #-}

-- | Variant of 'modify' that forces the result of the function.
--
-- >tally <- newIORef (0 :: Int)
-- >modify tally (+ 10)  -- add 10 to the tally
modify' :: (HasGet m v, HasPut m v, Monad m) => v a -> (a -> a) -> m ()
modify' v f = do
    x <- get v
    put v $! f x
{-# INLINE modify' #-}

-- | Write a new value and return the old value.
swap :: (HasGet m v, HasPut m v, Monad m) => v a -> a -> m a
swap var newVal = do
    old <- get var
    put var newVal
    return old
{-# INLINE swap #-}

------------------------------------------------------------------------
-- Infix aliases

infixr 2 $=
infixr 2 $~
infixr 2 $=!
infixr 2 $~!

-- | Infix alias for 'put'
($=) :: HasPut m v => v a -> a -> m ()
($=) = put
{-# INLINE ($=) #-}

-- | Infix alias for 'put''
($=!) :: HasPut m v => v a -> a -> m ()
($=!) = put'
{-# INLINE ($=!) #-}

-- | Infix alias for 'modify'
($~) :: (HasGet m v, HasPut m v, Monad m) => v a -> (a -> a) -> m ()
($~) = modify
{-# INLINE ($~) #-}

-- | Infix alias for 'modify''
($~!) :: (HasGet m v, HasPut m v, Monad m) => v a -> (a -> a) -> m ()
($~!) = modify'
{-# INLINE ($~!) #-}
