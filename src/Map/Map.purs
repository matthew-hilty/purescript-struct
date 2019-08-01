module Data.Struct.Map.Map
  ( map
  ) where

import Data.Struct.Map.RMap (class RMap, rmap)
import Type.RowList (class RowToList, RLProxy(RLProxy))

map
  :: forall f g l0 l1 p r0 r1 r2
   . RMap p f g l0 r0 l1 r1 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => g r0
  -> p (f r1) (f r2)
map =
  rmap
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
