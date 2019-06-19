module Data.Struct.RCompare
  ( class RCompare
  , rcompare
  ) where

import Prelude (Ordering)

import Record.Extra (class OrdRecord, compareRecord) as RecordExtra
import Type.Row (class RowToList, kind RowList)
import Type.Row (RLProxy) as TypeRow

class RCompare
  (f :: # Type -> Type)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  rcompare
    :: TypeRow.RLProxy l
    -> f r
    -> f r
    -> Ordering

instance rcompareRecord
  :: ( RecordExtra.OrdRecord l r
     , RowToList r l
     )
  => RCompare Record l r
  where
  rcompare _ = RecordExtra.compareRecord
