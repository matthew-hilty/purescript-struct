module Data.Struct.RGet
  ( class RGet
  , rget
  ) where

import Data.Symbol (class IsSymbol, SProxy)
import Record (get) as Record
import Type.Row (class Cons, kind RowList)
import Type.Row (RLProxy) as TypeRow

class RGet
  (f :: # Type -> Type)
  (g :: Symbol -> Type)
  (s :: Symbol)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  rget :: forall r' v. Cons s v r' r => TypeRow.RLProxy l -> g s -> f r -> v

instance rgetRecord :: IsSymbol s => RGet Record SProxy s l r where
  rget _ = Record.get
