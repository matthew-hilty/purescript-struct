module Data.Struct.Expand.Expand
  ( expand
  ) where

import Data.Struct.Expand.RExpand (class RExpand, rexpand)
import Type.Row (class Union)
import Type.RowList (class RowToList, RLProxy(RLProxy))

expand
  :: forall f l0 l1 p r r0 r1
   . RExpand p f l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => Union r0 r r1
  => p (f r0) (f r1)
expand =
  rexpand
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
