module Data.Struct.Eval.Eval
  ( eval
  ) where

import Data.Struct.Eval.REval (class REval, reval)
import Type.RowList (class RowToList, RLProxy(RLProxy))

eval
  :: forall f l0 l1 p r0 r1
   . REval p f l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => p (f r0) (f r1)
  -> f r0
  -> f r1
eval =
  reval
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
