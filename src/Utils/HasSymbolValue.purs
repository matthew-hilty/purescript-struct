module Data.Struct.Utils.HasSymbolValue
  ( class HasSymbolValue
  , class HasSymbolValue_
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

class HasSymbolValue (l :: RowList) (s :: Symbol) (b :: Boolean) | l s -> b

instance hasSymbolValue :: HasSymbolValue_ l s b True => HasSymbolValue l s b

class HasSymbolValue_
  (l :: RowList)
  (s :: Symbol)
  (b :: Boolean)
  (continue :: Boolean)
  | l s -> b

instance hasSymbolValue_False :: HasSymbolValue_ l s False False

instance hasSymbolValue_Nil :: HasSymbolValue_ Nil s False True

instance hasSymbolValue_Cons
  :: ( HasSymbolValue_ l' s1 output' uneq
     , Equals s s1 eq
     , Not eq uneq
     , If eq
         (BProxy True)
         (BProxy output')
         (BProxy output)
     )
  => HasSymbolValue_ (Cons s0 (f s) l') s1 output True
