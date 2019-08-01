module Data.Struct.Contract.RContract
  ( class RContract
  , rcontract
  ) where

import Data.Struct.Contract.GContract (class GContract)
import Data.Struct.Contract.GContract (gContract) as Contract
import Type.Proxying (class RProxying, rProxy)
import Type.Row (class Union)
import Type.RowList (class RowToList, RLProxy(RLProxy), kind RowList)

class RContract
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rcontract
    :: forall (g :: RowList -> Type) r
     . Union r1 r r0
    => g l0
    -> g l1
    -> p (f r0) (f r1)

instance rcontractRecord
  :: ( GContract p Record l0 r0 l1 r1 l2 r2
     , RowToList r0 l0
     , Union r2 r0 r1
     )
  => RContract p Record l1 r1 l2 r2
  where
  rcontract _ _ =
    Contract.gContract
      (RLProxy :: RLProxy l0)
      (RLProxy :: RLProxy l1)
      (RLProxy :: RLProxy l2)

else instance rcontractRProxying
  :: RProxying f r1
  => RContract Function f l0 r0 l1 r1
  where
  rcontract _ _ _ = rProxy
