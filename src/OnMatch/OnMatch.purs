module Data.Struct.OnMatch.OnMatch
  ( onMatch
  ) where

import Data.Struct.OnMatch.ROnMatch (class ROnMatch, ronMatch)
import Type.Row (class Union)
import Type.RowList (class RowToList, RLProxy(RLProxy))

onMatch
  :: forall f g l0 l1 l2 l3 r0 r1 r2 r3 v
   . ROnMatch f g v l0 r0 l1 r1 l2 r2 l3 r3
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList r3 l3
  => Union r1 r2 r3
  => f r0
  -> (g r2 -> v)
  -> g r3
  -> v
onMatch =
  ronMatch
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
    (RLProxy :: RLProxy l2)
    (RLProxy :: RLProxy l3)
