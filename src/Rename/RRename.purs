module Data.Struct.Rename.RRename
  ( class RRename
  , rrename
  ) where

import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Record (rename) as Record
import Record.Builder (Builder)
import Record.Builder (rename) as Builder
import Type.Proxying (class RProxying, class SProxying, rProxy)
import Type.Row (class Cons, class Lacks)
import Type.RowList (kind RowList)
import Unsafe.Coerce (unsafeCoerce)

class RRename
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s0 :: Symbol)
  (s1 :: Symbol)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rrename
    :: forall (h :: RowList -> Type) r v
     . Cons s0 v r r0
    => Cons s1 v r r1
    => Lacks s0 r
    => Lacks s0 r1
    => Lacks s1 r
    => Lacks s1 r0
    => h l0
    -> h l1
    -> g s0
    -> g s1
    -> p (f r0) (f r1)

instance rrenameBuilder
  :: ( IsSymbol s0
     , IsSymbol s1
     , SProxying g s0
     , SProxying g s1
     )
  => RRename Builder Record g s0 s1 l0 r0 l1 r1
  where
  rrename _ _ _ _ =
    Builder.rename
      (SProxy :: SProxy s0)
      (SProxy :: SProxy s1)

instance rrenameRecord
  :: ( IsSymbol s0
     , IsSymbol s1
     , SProxying g s0
     , SProxying g s1
     )
  => RRename Function Record g s0 s1 l0 r0 l1 r1
  where
  rrename _ _ _ _ =
    Record.rename
      (SProxy :: SProxy s0)
      (SProxy :: SProxy s1)

else instance rrenameVariant
  :: ( IsSymbol s0
     , IsSymbol s1
     , SProxying g s0
     , SProxying g s1
     )
  => RRename Function Variant g s0 s1 l0 r0 l1 r1
  where
  rrename _ _ _ _ =
    Variant.on
      (SProxy :: SProxy s0)
      (Variant.inj (SProxy :: SProxy s1))
      unsafeCoerce

else instance rrenameRProxying
  :: RProxying f r1
  => RRename Function f g s0 s1 l0 r0 l1 r1
  where
  rrename _ _ _ _ _ = rProxy
