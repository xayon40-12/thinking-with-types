module Cont.ContT where

import Cont
import Control.Monad (ap)
import Control.Monad.Trans
import GHC.Base (liftM)

newtype ContT m a = ContT {runContT :: m (Cont a)}

instance Monad m => Monad (ContT m) where
  (ContT x) >>= f = ContT $ x >>= runContT . flip unCont f

instance Monad m => Applicative (ContT m) where
  pure = ContT . return . cont
  (<*>) = ap

instance Monad m => Functor (ContT m) where
  fmap = liftM

instance MonadTrans ContT where
  lift = ContT . fmap cont
