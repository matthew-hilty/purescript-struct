module Data.Struct.Set.Set
  ( set
  ) where

import Data.Struct.Set.RSet (class RSet, rset)
import Type.Row (class Cons)
import Type.RowList (class RowToList, RLProxy(RLProxy))

set
  :: forall f g l0 l1 p r0 r1 r s v0 v1
   . Cons s v0 r r0
  => Cons s v1 r r1
  => RowToList r0 l0
  => RowToList r1 l1
  => RSet p f g s l0 r0 l1 r1
  => g s
  -> v1
  -> p (f r0) (f r1)
set =
  rset
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
