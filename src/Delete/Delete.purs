module Data.Struct.Delete.Delete
  ( delete
  ) where

import Data.Struct.Delete.RDelete (class RDelete, rdelete)
import Type.Row (class Cons, class Lacks)
import Type.RowList (class RowToList, RLProxy(RLProxy))

delete
  :: forall f g l0 l1 p r0 r1 s v
   . Cons s v r1 r0
  => Lacks s r1
  => RDelete p f g s l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => g s
  -> p (f r0) (f r1)
delete =
  rdelete
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
