module Data.Struct.Utils.Record
  ( singleton
  ) where

import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Record (insert)
import Type.Proxying (class SProxying)
import Type.Row (class Cons, class Lacks)

singleton
  :: forall r g l a
   . IsSymbol l
  => Cons l a () r
  => Lacks l ()
  => SProxying g l
  => g l
  -> a
  -> Record r
singleton l a = insert (SProxy :: SProxy l) a {}
