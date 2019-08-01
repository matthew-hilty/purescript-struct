module Data.Struct.Rename.Rename
  ( rename
  ) where

import Data.Struct.Rename.RRename (class RRename, rrename)
import Type.Row (class Cons, class Lacks)
import Type.RowList (class RowToList, RLProxy(RLProxy))

rename
  :: forall f g l0 l1 p r r0 r1 s0 s1 v
   . Cons s0 v r r0
  => Cons s1 v r r1
  => Lacks s0 r
  => Lacks s0 r1
  => Lacks s1 r
  => Lacks s1 r0
  => RowToList r0 l0
  => RowToList r1 l1
  => RRename p f g s0 s1 l0 r0 l1 r1
  => g s0
  -> g s1
  -> p (f r0) (f r1)
rename =
  rrename
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
