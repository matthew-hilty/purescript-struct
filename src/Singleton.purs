module Data.Struct.Singleton
  ( module Data.Struct.Singleton.RSingleton
  , singleton
  ) where

import Data.Struct.Singleton.RSingleton (class RSingleton, rsingleton)
import Type.Row (class Cons, class Lacks)

singleton
  :: forall f g r s v
   . Cons s v () r
  => Lacks s ()
  => RSingleton f g s
  => g s
  -> v
  -> f r
singleton = rsingleton
