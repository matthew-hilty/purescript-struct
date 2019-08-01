module Data.Struct.Merge.RMerge
  ( class RMerge
  , rmerge
  ) where

import Control.Subcategory.Restrictable (restrict)
import Record (merge) as Record
import Record.Builder (Builder, build)
import Record.Builder (merge) as Builder
import Type.Row (class Nub, class Union, RProxy(RProxy))
import Type.RowList (kind RowList)

class RMerge
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
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
  rmerge
    :: forall (g :: RowList -> Type) r3
     . Nub r2 r3
    => Union r0 r1 r2
    => g l0
    -> g l1
    -> g l2
    -> f r0
    -> p (f r1) (f r3)

instance rmergeBuilder
  :: Union r1 r0 r2
  => RMerge Builder Record l0 r0 l1 r1 l2 r2
  where
  rmerge _ _ _ record0 =
    -- Whereas the first argument of `Record.merge` has precedence,
    -- the second argument of `Builder.merge` has precedence.
    restrict \record1 ->
      build (Builder.merge record1) record0

instance rmergeRecord :: RMerge Function Record l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ = Record.merge

instance rmergeRProxy :: RMerge Function RProxy l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ _ _ = RProxy
