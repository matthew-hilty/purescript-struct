module Data.Struct.Equal.REqual
  ( class REqual
  , requal
  ) where

import Prelude (eq)

import Data.Variant (class VariantEqs, Variant)
import Data.Variant.Internal (class VariantTags)
import Record (class EqualFields, equal) as Record
import Type.Proxying (class RProxying)
import Type.RowList (class RowToList, kind RowList)

class REqual
  (f :: # Type -> Type)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  requal :: forall (g :: RowList -> Type). g l -> f r -> f r -> Boolean

instance requalRecord
  :: ( Record.EqualFields l r
     , RowToList r l
     )
  => REqual Record l r
  where
  requal _ = Record.equal

else instance requalVariant
  :: ( RowToList r l
     , VariantEqs l
     , VariantTags l
     )
  => REqual Variant l r
  where
  requal _ = eq

else instance requalRProxying :: RProxying f r => REqual f l r where
  requal _ _ _ = true
