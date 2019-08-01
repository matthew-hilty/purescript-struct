module Data.Struct.Utils.ReifyKeyAndValueSymbols
  ( class ReifyKeyAndValueSymbols
  , reifyKeyAndValueSymbols
  , reifyKeyAndValueSymbols'
  ) where

import Prelude (mempty)

import Data.List (List, (:))
import Data.Tuple (Tuple(Tuple))
import Type.Data.RowList (RLProxy(RLProxy))
import Type.Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Type.RowList (class RowToList, Cons, Nil, kind RowList)

class ReifyKeyAndValueSymbols (l :: RowList) where
  reifyKeyAndValueSymbols'
    :: forall (f :: RowList -> Type)
     . f l
    -> List (Tuple String String)

instance reifyKeyAndValueSymbolsNil :: ReifyKeyAndValueSymbols Nil where
  reifyKeyAndValueSymbols' = mempty

instance reifyKeyAndValueSymbolsCons
  :: ( IsSymbol s0
     , IsSymbol s1
     , ReifyKeyAndValueSymbols l'
     )
  => ReifyKeyAndValueSymbols (Cons s0 (f s1) l')
  where
  reifyKeyAndValueSymbols' _ = (Tuple string0 string1) : rest
    where
    string0 = reflectSymbol (SProxy :: SProxy s0)
    string1 = reflectSymbol (SProxy :: SProxy s1)
    rest = reifyKeyAndValueSymbols' (RLProxy :: RLProxy l')

reifyKeyAndValueSymbols
  :: forall f l r
   . ReifyKeyAndValueSymbols l
  => RowToList r l
  => f r
  -> List (Tuple String String)
reifyKeyAndValueSymbols _ = reifyKeyAndValueSymbols' (RLProxy :: RLProxy l)
