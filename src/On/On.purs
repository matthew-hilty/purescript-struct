module Data.Struct.On.On
  ( on
  ) where

import Data.Struct.On.ROn (class ROn, ron)
import Type.Row (class Cons)
import Type.RowList (class RowToList, RLProxy(RLProxy))

on
  :: forall a b f g l0 l1 r0 r1 s
   . Cons s a r0 r1
  => ROn f g s l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => g s
  -> (a -> b)
  -> (f r0 -> b)
  -> f r1
  -> b
on =
  ron
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
