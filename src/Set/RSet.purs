module Data.Struct.Set.RSet
  ( class RSet
  , rset
  ) where

import Prelude (($))

import Control.Subcategory.Restrictable (restrict)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Record (set) as Record
import Record.Builder (Builder)
import Type.Proxying (class RProxying, class SProxying, rProxy, reflectSymbol)
import Type.Row (class Cons)
import Type.RowList (kind RowList)

class RSet
  (p  :: Type -> Type -> Type)
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
    :: forall (h :: RowList -> Type) r v0 v1
     . Cons s v0 r r0
    => Cons s v1 r r1
    => h l0
    -> h l1
    -> g s
    -> v1
    -> p (f r0) (f r1)

instance rsetBuilder
  :: ( IsSymbol s
     , SProxying g s
     )
  => RSet Builder Record g s l0 r0 l1 r1
  where
  rset _ _ s value =
    restrict $ unsafeBuilderSet (reflectSymbol s) value

instance rsetRecord
  :: ( IsSymbol s
     , SProxying g s
     )
  => RSet Function Record g s l0 r0 l1 r1
  where
  rset _ _ _ = Record.set (SProxy :: SProxy s)

else instance rsetVariant
  :: ( IsSymbol s
     , SProxying g s
     )
  => RSet Function Variant g s l0 r0 l1 r1
  where
  rset _ _ _ value _ =
    Variant.inj (SProxy :: SProxy s) value

else instance rsetRProxying
  :: RProxying f r1
  => RSet Function f g s l0 r0 l1 r1
  where
  rset _ _ _ _ _ = rProxy

foreign import unsafeBuilderSet
  :: forall r0 r1 v
   . String
  -> v
  -> Record r0
  -> Record r1
