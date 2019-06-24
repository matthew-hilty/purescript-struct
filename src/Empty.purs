module Data.Struct.Empty
  ( module Data.Struct.Empty.REmpty
  , empty
  ) where

import Data.Struct.Empty.REmpty (class REmpty, rempty)

empty :: forall f. REmpty f => f ()
empty = rempty
