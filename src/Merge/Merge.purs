module Data.Struct.Merge.Merge
  ( merge
  ) where

import Data.Struct.Merge.RMerge (class RMerge, rmerge)
import Type.Row (class Nub, class Union)
import Type.RowList (class RowToList, RLProxy(RLProxy))

merge
  :: forall f l0 l1 l2 p r0 r1 r2 r3
   . Nub r2 r3
  => RMerge p f l0 r0 l1 r1 l2 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Union r0 r1 r2
  => f r0
  -> p (f r1) (f r3)
merge =
  rmerge
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
    (RLProxy :: RLProxy l2)
