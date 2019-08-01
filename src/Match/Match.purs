module Data.Struct.Match.Match
  ( match
  ) where

import Data.Struct.Match.RMatch (class RMatch, rmatch)
import Type.RowList (class RowToList, RLProxy(RLProxy))

match
  :: forall f g l0 l1 r0 r1 v
   . RMatch f g v l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => f r0
  -> g r1
  -> v
match =
  rmatch
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
