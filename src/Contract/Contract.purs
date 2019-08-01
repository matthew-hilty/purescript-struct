module Data.Struct.Contract.Contract
  ( contract
  ) where

import Data.Struct.Contract.GContract (class GContract, gContract)
import Type.Row (class Union)
import Type.RowList (class RowToList, RLProxy(RLProxy))

contract
  :: forall f l0 l1 l2 p r0 r1 r2
   . GContract p f l0 r0 l1 r1 l2 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Union r2 r0 r1
  => p (f r1) (f r2)
contract =
  gContract
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
    (RLProxy :: RLProxy l2)
