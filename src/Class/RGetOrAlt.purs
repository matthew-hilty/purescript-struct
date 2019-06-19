module Data.Struct.RGetOrAlt
  ( class RGetOrAlt
  , rgetOrAlt
  ) where

import Prelude (pure, ($))

import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Data.Variant (prj) as Variant
import Record (get) as Record
import Type.Row (class Cons, kind RowList)
import Type.Row (RLProxy) as TypeRow

class RGetOrAlt
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  rgetOrAlt
    :: forall h r' v
     . Alternative h
    => Cons s v r' r
    => TypeRow.RLProxy l
    -> g s
    -> f r
    -> h v

instance rgetOrAltRecord :: IsSymbol s => RGetOrAlt Record SProxy s l r where
  rgetOrAlt _ s record = pure $ Record.get s record

instance rgetOrAltVariant :: IsSymbol s => RGetOrAlt Variant SProxy s l r where
  rgetOrAlt _ = Variant.prj
