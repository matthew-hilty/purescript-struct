module Data.Struct.RSet
  ( class RSet
  , rset
  ) where

import Prelude (const, (<<<))

import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Record (set) as Record
import Type.Row (class Cons, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy) as TypeRow
import Unsafe.Coerce (unsafeCoerce)

class RSet
  (f :: # Type -> Type)
  (g :: Symbol -> Type)
  (s :: Symbol)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rset
    :: forall r v0 v1
     . Cons s v0 r r0
    => Cons s v1 r r1
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> g s
    -> v1
    -> f r0
    -> f r1

instance rsetRecord :: IsSymbol s => RSet Record SProxy s l0 r0 l1 r1 where
  rset _ _ = Record.set

instance rsetRProxy :: RSet RProxy g s l0 r0 l1 r1 where
  rset _ _ _ _ _ = RProxy

instance rsetVariant :: IsSymbol s => RSet Variant SProxy s l0 r0 l1 r1 where
  rset _ _ s v =
    Variant.on
      s
      (Variant.inj s <<< const v)
      unsafeCoerce
