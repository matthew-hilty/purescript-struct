module Data.Struct.Map.RMap
  ( class RMap
  , rmap
  ) where

import Data.Struct.Map.GMap (class GMap, gMap)
import Data.Variant (Variant)
import Type.Proxying (class RProxying, rProxy)
import Type.RowList (RowList)

class RMap
  (p  :: Type -> Type -> Type)
  (f  :: Row Type -> Type)
  (g  :: Row Type -> Type)
  (l0 :: RowList Type)
  (r0 :: Row Type)
  (l1 :: RowList Type)
  (r1 :: Row Type)
  (r2 :: Row Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  rmap
    :: forall (h :: RowList Type -> Type)
     . h l0
    -> h l1
    -> g r0
    -> p (f r1) (f r2)

instance rmapRecord
  :: GMap p Record l0 r0 l1 r1 r2
  => RMap p Record Record l0 r0 l1 r1 r2
  where
  rmap = gMap

else instance rmapVariant
  :: GMap p Variant l0 r0 l1 r1 r2
  => RMap p Variant Record l0 r0 l1 r1 r2
  where
  rmap = gMap

else instance rmapRProxying
  :: RProxying f r2
  => RMap Function f g l0 r0 l1 r1 r2
  where
  rmap _ _ _ _ = rProxy
