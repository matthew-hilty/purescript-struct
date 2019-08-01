module Data.Struct.Utils.HasSymbol
  ( class HasSymbol
  , class HasSymbol_
  ) where

import Type.Data.Boolean
  ( class If
  , class Not
  , BProxy
  , False
  , True
  , kind Boolean
  )
import Type.Data.Symbol (class Equals)
import Type.RowList (Cons, Nil, kind RowList)

class HasSymbol (l :: RowList) (s :: Symbol) (b :: Boolean) | l s -> b

instance hasSymbol' :: HasSymbol_ l s b True => HasSymbol l s b

class HasSymbol_
  (l :: RowList)
  (s :: Symbol)
  (b :: Boolean)
  (continue :: Boolean)
  | l s -> b

instance hasSymbol_False :: HasSymbol_ l s False False

instance hasSymbol_Nil :: HasSymbol_ Nil s False True

instance hasSymbol_Cons
  :: ( HasSymbol_ l' s1 output' uneq
     , Equals s0 s1 eq
     , Not eq uneq
     , If eq
         (BProxy True)
         (BProxy output')
         (BProxy output)
     )
  => HasSymbol_ (Cons s0 v l') s1 output True
