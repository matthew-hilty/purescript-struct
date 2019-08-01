module Data.Struct.Get.Get
  ( get
  ) where

import Data.Struct.Get.RGet (class RGet, rget)
import Type.Row (class Cons)
import Type.RowList (class RowToList, RLProxy(RLProxy))

get
  :: forall f g l r r' s v
   . Cons s v r' r
  => RGet f g s l r
  => RowToList r l
  => g s
  -> f r
  -> v
get =
  rget (RLProxy :: RLProxy l)
