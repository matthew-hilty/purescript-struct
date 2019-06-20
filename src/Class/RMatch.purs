module Data.Struct.RMatch
  ( class RMatch
  , rmatch
  ) where

import Data.Variant (class VariantMatchCases, Variant)
import Data.Variant ( match) as Variant
import Type.Proxying (class RLProxying)
import Type.Row (class RowToList, class Union, kind RowList)

class RMatch
  (f  :: # Type -> Type)
  (g  :: # Type -> Type)
  (v  :: Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  where
  rmatch
    :: forall h
     . RLProxying h l0
    => RLProxying h l1
    => RLProxying h l2
    => Union r1 () r2
    => h l0
    -> h l1
    -> h l2
    -> f r0
    -> g r2
    -> v

instance rmatchVariant
  :: ( RowToList r0 l0
     , VariantMatchCases l0 r1 v
     )
  => RMatch Record Variant v l0 r0 l1 r1 l2 r2
  where
  rmatch _ _ _ = Variant.match
