module Data.Struct.ContractOrAlt.ContractOrAlt
  ( contractOrAlt
  ) where

import Control.Alternative (class Alternative)
import Data.Struct.ContractOrAlt.RContractOrAlt (class RContractOrAlt, rcontractOrAlt)
import Record.Extra (class Keys) as RecordExtra
import Type.Row (class Union)
import Type.RowList (class RowToList, RLProxy(RLProxy))

contractOrAlt
  :: forall f h l0 l1 p r r0 r1
   . Alternative h
  => RecordExtra.Keys l1
  => RContractOrAlt p f l0 r0 l1 r1
  => RowToList r0 l0
  => RowToList r1 l1
  => Union r1 r r0
  => p (f r0) (h (f r1))
contractOrAlt =
  rcontractOrAlt
    (RLProxy :: RLProxy l0)
    (RLProxy :: RLProxy l1)
