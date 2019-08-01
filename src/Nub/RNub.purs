module Data.Struct.Nub.RNub
  ( class RNub
  , rnub
  ) where

import Data.Variant (Variant)
import Record (nub) as Record
import Record.Builder (Builder)
import Record.Builder (nub) as Builder
import Type.Proxying (class RProxying, rProxy)
import Type.Row (class Nub)
import Type.RowList (kind RowList)
import Unsafe.Coerce (unsafeCoerce)

class RNub
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rnub
    :: forall (g :: RowList -> Type)
     . Nub r0 r1
    => g l0
    -> g l1
    -> p (f r0) (f r1)

instance rnubBuilder :: RNub Builder Record l0 r0 l1 r1 where
  rnub _ _ = Builder.nub

instance rnubRecord :: RNub Function Record l0 r0 l1 r1 where
  rnub _ _ = Record.nub

else instance rnubVariant :: RNub Function Variant l0 r0 l1 r1 where
  rnub _ _ = unsafeCoerce

else instance rnubRProxying
  :: RProxying f r1
  => RNub Function f l0 r0 l1 r1
  where
  rnub _ _ _ = rProxy
