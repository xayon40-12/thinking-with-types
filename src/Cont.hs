{-# LANGUAGE RankNTypes #-}

module Cont where

newtype Cont a = Cont {unCont :: forall r. (a -> r) -> r}

runCont :: Cont a -> a
runCont = flip unCont id

cont :: a -> Cont a
cont a = Cont $ \callback -> callback a

instance Functor Cont where
  fmap f = pure . flip unCont f

instance Applicative Cont where
  pure a = cont a
  fab <*> fa = cont $ unCont fa (unCont fab id)

instance Monad Cont where
  ma >>= amb = unCont ma amb

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532

testCont :: String
testCont = runCont $ do
  version <- Cont withVersionNumber
  date <- Cont withTimestamp
  return $ show version ++ "-" ++ show date
