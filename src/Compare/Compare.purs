module Data.Struct.Compare.Compare
  ( compare
  ) where

import Prelude (Ordering)

import Data.Struct.Compare.RCompare (class RCompare, rcompare)
import Type.RowList (class RowToList, RLProxy(RLProxy))

compare
  :: forall f l r
   . RowToList r l
  => RCompare f l r
  => f r
  -> f r
  -> Ordering
compare =
  rcompare
    (RLProxy :: RLProxy l)
