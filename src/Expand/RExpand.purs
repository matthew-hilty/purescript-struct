module Data.Struct.Expand.RExpand
  ( class RExpand
  , rexpand
  ) where

import Data.Variant (Variant)
import Data.Variant (expand) as Variant
import Type.Proxying (class RProxying, rProxy)
import Type.Row (class Union)
import Type.RowList (RowList)

class RExpand
  (p  :: Type -> Type -> Type)
  (f  :: Row Type -> Type)
  (l0 :: RowList Type)
  (r0 :: Row Type)
  (l1 :: RowList Type)
  (r1 :: Row Type)
  | l0 -> r0
  , l1 -> r1
  where
  rexpand
    :: forall (g :: RowList Type -> Type) r
     . Union r0 r r1
    => g l0
    -> g l1
    -> p (f r0) (f r1)

instance rexpandVariant :: RExpand Function Variant l0 r0 l1 r1 where
  rexpand _ _ = Variant.expand

else instance rexpandRProxying
  :: RProxying f r1
  => RExpand Function f l0 r0 l1 r1
  where
  rexpand _ _ _ = rProxy
