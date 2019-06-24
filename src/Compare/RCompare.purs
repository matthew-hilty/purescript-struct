module Data.Struct.Compare.RCompare
  ( class RCompare
  , rcompare
  ) where

import Prelude (Ordering)

import Record.Extra (class OrdRecord, compareRecord) as RecordExtra
import Type.Proxying (class RLProxying)
import Type.Row (class RowToList, kind RowList)

class RCompare
  (f :: # Type -> Type)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  rcompare
    :: forall g
     . RLProxying g l
    => g l
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
