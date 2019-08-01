module Data.Struct.DisjointUnion.RDisjointUnion
  ( class RDisjointUnion
  , rdisjointUnion
  ) where

import Record (disjointUnion) as Record
import Record.Builder (Builder)
import Record.Builder (disjointUnion) as Builder
import Type.Proxying (class RProxying, rProxy)
import Type.Row (class Nub, class Union)
import Type.RowList (kind RowList)

class RDisjointUnion
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
  rdisjointUnion
    :: forall (g :: RowList -> Type)
     . Nub r2 r2
    => Union r0 r1 r2
    => g l0
    -> g l1
    -> g l2
    -> f r0
    -> p (f r1) (f r2)

instance rdisjointUnionBuilder
  :: RDisjointUnion Builder Record l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ = Builder.disjointUnion

instance rdisjointUnionRecord
  :: RDisjointUnion Function Record l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ = Record.disjointUnion

else instance rdisjointUnionRProxying
  :: RProxying f r2
  => RDisjointUnion Function f l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ _ _= rProxy
