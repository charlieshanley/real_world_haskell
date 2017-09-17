{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module MaybeT (MaybeT(..)) where

import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Writer (MonadWriter(..))

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- First, MaybeT is a monad transformer (so we can access underlying monad)
instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

-- Next, it is a functor and applicative functor, so long as the underlying
-- type is a functor and monad.
instance (Functor f) => Functor (MaybeT f) where
    fmap f = MaybeT . (fmap . fmap) f . runMaybeT

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = lift . return
    mf <*> mx = MaybeT $
        runMaybeT mf >>= \mb_f ->
        case mb_f of
            Nothing -> return Nothing
            Just f  ->
                runMaybeT mx >>= \mb_x ->
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))


-- Next, MaybeT is a monad so long as its underlying type is as well
instance (Monad m) => Monad (MaybeT m) where
    return = returnMT
    (>>=) = bindMT
    fail = failMT

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT = MaybeT . return . Just

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT


-- Finally, instantiate MaybeT into the interface-typeclasses of many other
-- monads for convenient access to the functionality of monads/monad transformers
-- below in the stack

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put

instance (MonadReader r m) => MonadReader r (MaybeT m) where
    ask = lift ask
    local = mapMaybeT . local
    reader = lift . reader
