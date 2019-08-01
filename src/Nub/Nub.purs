module Data.Struct.Nub.Nub
  ( nub
  ) where

import Data.Struct.Nub.RNub (class RNub, rnub)
import Type.Row (class Nub)
import Type.RowList (class RowToList, RLProxy(RLProxy))

nub
  :: forall f l0 l1 p r0 r1
   . Nub r0 r1
  => RNub p f l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => p (f r0) (f r1)
nub =
  rnub
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
