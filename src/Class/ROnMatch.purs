module Data.Struct.ROnMatch
  ( class ROnMatch
  , ronMatch
  ) where

import Data.Variant (class VariantMatchCases, Variant)
import Data.Variant (onMatch) as Variant
import Type.Row (class RowToList, class Union, kind RowList)
import Type.Row (RLProxy) as TypeRow

class ROnMatch
  (f  :: # Type -> Type)
  (g  :: # Type -> Type)
  (v  :: Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  (l3 :: RowList)
  (r3 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  , l3 -> r3
  where
  ronMatch
    :: Union r1 r2 r3
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> TypeRow.RLProxy l3
    -> f r0
    -> (g r2 -> v)
    -> g r3
    -> v

instance ronMatchVariant
  :: ( RowToList r0 l0
     , VariantMatchCases l0 r1 v
     )
  => ROnMatch Record Variant v l0 r0 l1 r1 l2 r2 l3 r3
  where
  ronMatch _ _ _ _ = Variant.onMatch
