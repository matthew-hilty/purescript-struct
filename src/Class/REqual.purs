module Data.Struct.REqual
  ( class REqual
  , requal
  ) where

import Prelude (eq)

import Data.Variant (class VariantEqs, Variant)
import Data.Variant.Internal (class VariantTags)
import Record (class EqualFields, equal) as Record
import Type.Row (class RowToList, RProxy, kind RowList)
import Type.Row (RLProxy) as TypeRow

class REqual
  (f :: # Type -> Type)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  requal :: TypeRow.RLProxy l -> f r -> f r -> Boolean

instance requalRecord
  :: ( Record.EqualFields l r
     , RowToList r l
     )
  => REqual Record l r where
  requal _ = Record.equal

instance requalRProxy :: REqual RProxy l r where
  requal _ _ _ = true

instance requalVariant
  :: ( RowToList r l
     , VariantEqs l
     , VariantTags l
     )
  => REqual Variant l r where
  requal _ = eq
