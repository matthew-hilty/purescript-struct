module Data.Struct.Insert.Insert
  ( insert
  ) where

import Data.Struct.Insert.RInsert (class RInsert, rinsert)
import Type.Row (class Cons, class Lacks)
import Type.RowList (class RowToList, RLProxy(RLProxy))

insert
  :: forall f g l0 l1 p r0 r1 s v
   . Cons s v r0 r1
  => Lacks s r0
  => RInsert p f g s l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => g s
  -> v
  -> p (f r0) (f r1)
insert =
  rinsert
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
