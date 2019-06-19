module Data.Struct.RMerge
  ( class RMerge
  , rmerge
  ) where

import Record (merge) as Record
import Record.Builder (Builder)
import Record.Builder (merge) as Builder
import Type.Row (class Nub, class Union, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy) as TypeRow

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
    :: forall r3
     . Nub r2 r3
    => Union r0 r1 r2
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> f r0
    -> p (f r1) (f r3)

instance rmergeBuilder
  :: Union r1 r0 r2
  => RMerge Builder Record l0 r0 l1 r1 l2 r2
  where
  rmerge _ _ _ = Builder.merge

instance rmergeRecord :: RMerge Function Record l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ = Record.merge

instance rmergeRProxy :: RMerge Function RProxy l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ _ _ = RProxy
