module Data.Struct.Get.RGet
  ( class RGet
  , rget
  ) where

import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Record (get) as Record
import Type.Proxying (class SProxying)
import Type.Row (class Cons)
import Type.RowList (kind RowList)

class RGet
  (f :: # Type -> Type)
  (g :: Symbol -> Type)
  (s :: Symbol)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  rget
    :: forall (h :: RowList -> Type) r' v
     . Cons s v r' r
    => h l
    -> g s
    -> f r
    -> v

instance rgetRecord
  :: ( IsSymbol s
     , SProxying g s
     )
  => RGet Record g s l r
  where
  rget _ _ = Record.get (SProxy :: SProxy s)
