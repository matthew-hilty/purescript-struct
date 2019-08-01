module Data.Struct.On.ROn
  ( class ROn
  , ron
  ) where

import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Variant (Variant)
import Data.Variant (on) as Variant
import Type.Proxying (class SProxying)
import Type.Row (class Cons)
import Type.RowList (kind RowList)

class ROn
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  ron
    :: forall a b (h :: RowList -> Type)
     . Cons s a r0 r1
    => h l0
    -> h l1
    -> g s
    -> (a -> b)
    -> (f r0 -> b)
    -> f r1
    -> b

instance ronVariant
  :: ( IsSymbol s
     , SProxying g s
     )
  => ROn Variant g s l0 r0 l1 r1
  where
  ron _ _ _ = Variant.on (SProxy :: SProxy s)
