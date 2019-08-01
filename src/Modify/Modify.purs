module Data.Struct.Modify.Modify
  ( modify
  ) where

import Data.Struct.Modify.RModify (class RModify, rmodify)
import Type.Row (class Cons)
import Type.RowList (class RowToList, RLProxy(RLProxy))

modify
  :: forall f g l0 l1 p r r0 r1 s v0 v1
   . Cons s v0 r r0
  => Cons s v1 r r1
  => RModify p f g s l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => g s
  -> (v0 -> v1)
  -> p (f r0) (f r1)
modify =
  rmodify
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
