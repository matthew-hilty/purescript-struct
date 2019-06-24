module Data.Struct.Empty.REmpty
  ( class REmpty
  , rempty
  ) where

import Type.Proxying (class RProxying, rProxy)

class REmpty (f :: # Type -> Type) where
  rempty :: f ()

instance remptyRecord :: REmpty Record where
  rempty = {}

else instance remptyRProxying :: RProxying g () => REmpty g where
  rempty = rProxy
