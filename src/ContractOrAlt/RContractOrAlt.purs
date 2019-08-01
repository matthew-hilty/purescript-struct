module Data.Struct.ContractOrAlt.RContractOrAlt
  ( class RContractOrAlt
  , rcontractOrAlt
  ) where

import Prelude (pure, ($))

import Control.Alternative (class Alternative)
import Data.Variant (Variant)
import Data.Variant (contract) as Variant
import Data.Variant.Internal (class Contractable)
import Record.Extra (class Keys, pick) as RecordExtra
import Type.Proxying (class RProxying, rProxy)
import Type.Row (class Union)
import Type.RowList (class ListToRow, class RowToList, kind RowList)

class RContractOrAlt
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rcontractOrAlt
    :: forall (g :: RowList -> Type) h r
     . Alternative h
    => Union r1 r r0
    => g l0
    -> g l1
    -> p (f r0) (h (f r1))

instance rcontractRecord
  :: ( RecordExtra.Keys l1
     , RowToList r1 l1
     , ListToRow l1 r1
     )
  => RContractOrAlt Function Record l0 r0 l1 r1
  where
  rcontractOrAlt _ _ record = pure $ RecordExtra.pick record

else instance rcontractVariant
  :: Contractable r0 r1
  => RContractOrAlt Function Variant l0 r0 l1 r1
  where
  rcontractOrAlt _ _ = Variant.contract

else instance rcontractRProxying
  :: RProxying f r1
  => RContractOrAlt Function f l0 r0 l1 r1 where
  rcontractOrAlt _ _ _ = pure rProxy
