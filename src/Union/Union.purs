module Data.Struct.Union.Union
  ( union
  ) where

import Data.Struct.Union.RUnion (class RUnion, runion)
import Type.Row (class Union)
import Type.RowList (class RowToList, RLProxy(RLProxy))

union
  :: forall f l0 l1 l2 p r0 r1 r2
   . RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => RUnion p f l0 r0 l1 r1 l2 r2
  => Union r0 r1 r2
  => f r0
  -> p (f r1) (f r2)
union =
  runion
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
    (RLProxy :: RLProxy l2)
