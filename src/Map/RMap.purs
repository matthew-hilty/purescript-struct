module Data.Struct.Map.RMap
  ( class RMap
  , rmap
  ) where

import Data.Struct.Map.GMap (class GMap, gMap)
import Data.Variant (Variant)
import Type.Proxying (class RLProxying, class RProxying, rProxy)
import Type.Row (kind RowList)

class RMap
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  rmap
    :: forall h
     . RLProxying h l0
    => RLProxying h l1
    => h l0
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
