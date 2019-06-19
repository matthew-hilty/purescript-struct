module Data.Struct.Utils.Record
  ( singleton
  ) where

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Type.Row (class ListToRow, Cons, Nil)

singleton
  :: forall r s v
   . IsSymbol s
  => ListToRow (Cons s v Nil) r
  => SProxy s
  -> v
  -> Record r
singleton sProxy value =
  unsafeSingleton (reflectSymbol sProxy) value

foreign import unsafeSingleton :: forall r a. String -> a -> Record r
