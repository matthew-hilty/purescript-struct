module Data.Struct.Singleton.RSingleton
  ( class RSingleton
  , rsingleton
  ) where

import Data.Symbol (class IsSymbol)
import Data.Struct.Utils.Record (singleton)
import Type.Proxying (class SProxying)
import Type.Row (class Cons, class Lacks)

class RSingleton
  (f :: Row Type -> Type)
  (g :: Symbol -> Type)
  (s :: Symbol)
  where
  rsingleton :: forall (r :: Row Type) (v :: Type). Cons s v () r => Lacks s (() :: Row Type) => g s -> v -> f r

instance rsingletonRecord
  :: ( IsSymbol s
     , SProxying g s
     )
  => RSingleton Record g s
  where
  rsingleton = singleton
