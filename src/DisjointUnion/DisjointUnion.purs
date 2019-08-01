module Data.Struct.DisjointUnion.DisjointUnion
  ( disjointUnion
  ) where

import Data.Struct.DisjointUnion.RDisjointUnion (class RDisjointUnion, rdisjointUnion)
import Type.Row (class Nub, class Union)
import Type.RowList (class RowToList, RLProxy(RLProxy))

disjointUnion
  :: forall f l0 l1 l2 p r0 r1 r2
   . Nub r2 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => RDisjointUnion p f l0 r0 l1 r1 l2 r2
  => Union r0 r1 r2
  => f r0
  -> p (f r1) (f r2)
disjointUnion =
  rdisjointUnion
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
    (RLProxy :: RLProxy l2)
