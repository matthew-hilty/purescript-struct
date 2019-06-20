module Data.Struct.RSet
  ( class RSet
  , rset
  ) where

import Prelude (const, (<<<))

import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Record (set) as Record
import Type.Proxying
  ( class RLProxying
  , class RProxying
  , class SProxying
  , rProxy
  )
import Type.Row (class Cons, kind RowList)
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
    :: forall h r v0 v1
     . Cons s v0 r r0
    => Cons s v1 r r1
    => RLProxying h l0
    => RLProxying h l1
    => h l0
    -> h l1
    -> g s
    -> v1
    -> f r0
    -> f r1

instance rsetRecord
  :: ( IsSymbol s
     , SProxying g s
     )
  => RSet Record g s l0 r0 l1 r1
  where
  rset _ _ _ = Record.set (SProxy :: SProxy s)

else instance rsetVariant
  :: ( IsSymbol s
     , SProxying g s
     )
  => RSet Variant g s l0 r0 l1 r1
  where
  rset _ _ _ v =
    Variant.on
      (SProxy :: SProxy s)
      (Variant.inj (SProxy :: SProxy s) <<< const v)
      unsafeCoerce

else instance rsetRProxying
  :: RProxying f r1
  => RSet f g s l0 r0 l1 r1
  where
  rset _ _ _ _ _ = rProxy
