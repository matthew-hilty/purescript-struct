module Data.Struct.Compare.RCompare
  ( class RCompare
  , rcompare
  ) where

import Prelude (Ordering)

import Record.Extra (class OrdRecord, compareRecord) as RecordExtra
import Type.RowList (class RowToList, RowList)

class RCompare
  (f :: Row Type -> Type)
  (l :: RowList Type)
  (r :: Row Type)
  | l -> r
  where
  rcompare
    :: forall (g :: RowList Type -> Type)
     . g l
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
