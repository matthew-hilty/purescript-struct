module Data.Struct.REmpty
  ( class REmpty
  , rempty
  ) where

import Type.Row (RProxy(RProxy))

class REmpty (f :: # Type -> Type) where
  rempty :: f ()

instance remptyRecord :: REmpty Record where
  rempty = {}

instance remptyRProxy :: REmpty RProxy where
  rempty = RProxy
