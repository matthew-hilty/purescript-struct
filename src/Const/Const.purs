module Data.Struct.Const.Const
  ( const
  ) where

import Data.Struct.Const.RConst (class RConst, rconst)
import Type.RowList (class RowToList, RLProxy(RLProxy))

const
  :: forall f l0 l1 p r0 r1
   . RConst p f l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => f r0
  -> p (f r1) (f r0)
const =
  rconst
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
