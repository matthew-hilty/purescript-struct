module Data.Struct.Contract.Contract
  ( contract
  ) where

import Data.Struct.Contract.GContract
  ( class GContract
  , gContract
  )
import Type.Row (class RowToList, class Union)
import Type.Row (RLProxy(RLProxy)) as TypeRow

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
    (TypeRow.RLProxy :: TypeRow.RLProxy l0)
    (TypeRow.RLProxy :: TypeRow.RLProxy l1)
    (TypeRow.RLProxy :: TypeRow.RLProxy l2)
