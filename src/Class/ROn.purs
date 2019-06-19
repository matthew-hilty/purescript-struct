module Data.Struct.ROn
  ( class ROn
  , ron
  ) where

import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Data.Variant (on) as Variant
import Type.Row (class Cons, kind RowList)
import Type.Row (RLProxy) as TypeRow

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
    :: forall a b
     . Cons s a r0 r1
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> g s
    -> (a -> b)
    -> (f r0 -> b)
    -> f r1
    -> b

instance ronVariant :: IsSymbol s => ROn Variant SProxy s l0 r0 l1 r1 where
  ron _ _ = Variant.on
