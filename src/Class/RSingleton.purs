module Data.Struct.RSingleton
  ( class RSingleton
  , rsingleton
  ) where

import Data.Symbol (class IsSymbol)
import Data.Struct.Utils.Record (singleton)
import Type.Proxying (class SProxying)
import Type.Row (class ListToRow, Cons , Nil)

class RSingleton
  (f :: # Type -> Type)
  (g :: Symbol -> Type)
  (s :: Symbol)
  where
  rsingleton :: forall r v. ListToRow (Cons s v Nil) r => g s -> v -> f r

instance rsingletonRecord
  :: ( IsSymbol s
     , SProxying g s
     )
  => RSingleton Record g s
  where
  rsingleton = singleton
