module Data.Struct.Match.RMatch
  ( class RMatch
  , rmatch
  ) where

import Data.Variant (class VariantMatchCases, Variant)
import Data.Variant ( match) as Variant
import Type.Row (class Union)
import Type.RowList (class RowToList, RowList)

class RMatch
  (f  :: Row Type -> Type)
  (g  :: Row Type -> Type)
  (v  :: Type)
  (l0 :: RowList Type)
  (r0 :: Row Type)
  (l1 :: RowList Type)
  (r1 :: Row Type)
  | l0 -> r0
  , l1 -> r1
  where
  rmatch
    :: forall (h :: RowList Type -> Type)
     . h l0
    -> h l1
    -> f r0
    -> g r1
    -> v

instance rmatchVariant
  :: ( RowToList r0 l0
     , Union r1 () r1
     , VariantMatchCases l0 r1 v
     )
  => RMatch Record Variant v l0 r0 l1 r1
  where
  rmatch _ _ = Variant.match
