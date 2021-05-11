{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HList where

import Data.Kind

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hlenght :: HList ts -> Int
hlenght HNil = 0
hlenght (_ :# as) = 1 + hlenght as

hhead :: HList (t ': ts) -> t
hhead (a :# _) = a

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All _ '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance (All Eq ts) => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

instance (All Show ts) => Show (HList ts) where
  show HNil = "[]"
  show (a :# as) = show a <> " : " <> show as

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  HNil <= HNil = True
  (a :# as) <= (b :# bs) = a <= b && as <= bs
