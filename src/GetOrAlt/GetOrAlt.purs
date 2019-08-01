module Data.Struct.GetOrAlt.GetOrAlt
  ( getOrAlt
  ) where

import Control.Alternative (class Alternative)
import Data.Struct.GetOrAlt.RGetOrAlt (class RGetOrAlt, rgetOrAlt)
import Type.Row (class Cons)
import Type.RowList (class RowToList, RLProxy(RLProxy))

getOrAlt
  :: forall f g h l r r' s v
   . Alternative h
  => Cons s v r' r
  => RowToList r l
  => RGetOrAlt f g s l r
  => g s
  -> f r
  -> h v
getOrAlt =
  rgetOrAlt (RLProxy :: RLProxy l)
