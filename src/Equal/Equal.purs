module Data.Struct.Equal.Equal
  ( equal
  ) where

import Data.Struct.Equal.REqual (class REqual, requal)
import Type.RowList (class RowToList, RLProxy(RLProxy))

equal
  :: forall f l r
   . REqual f l r
  => RowToList r l
  => f r
  -> f r
  -> Boolean
equal =
  requal (RLProxy :: RLProxy l)
