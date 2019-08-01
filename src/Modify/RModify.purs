module Data.Struct.Modify.RModify
  ( class RModify
  , rmodify
  ) where

import Prelude (identity, (<<<))

import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Record (modify) as Record
import Record.Builder (Builder)
import Record.Builder (modify) as Builder
import Type.Proxying (class RProxying, class SProxying, rProxy)
import Type.Row (class Cons)
import Type.RowList (kind RowList)
import Unsafe.Coerce (unsafeCoerce)

class RModify
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rmodify
    :: forall (h :: RowList -> Type) r v0 v1
     . Cons s v0 r r0
    => Cons s v1 r r1
    => h l0
    -> h l1
    -> g s
    -> (v0 -> v1)
    -> p (f r0) (f r1)

instance rmodifyBuilder
  :: ( IsSymbol s
     , SProxying g s
     )
  => RModify Builder Record g s l0 r0 l1 r1
  where
  rmodify _ _ _ = Builder.modify (SProxy :: SProxy s)

instance rmodifyRecord
  :: ( IsSymbol s
     , SProxying g s
     )
  => RModify Function Record g s l0 r0 l1 r1
  where
  rmodify _ _ _ = Record.modify (SProxy :: SProxy s)

else instance rmodifyVariant
  :: ( IsSymbol s
     , SProxying g s
     )
  => RModify Function Variant g s l0 r0 l1 r1
  where
  rmodify _ _ _ f =
    Variant.on
      (SProxy :: SProxy s)
      (Variant.inj (SProxy :: SProxy s) <<< f)
      (unsafeCoerce <<< identity)

else instance rmodifyRProxying
  :: RProxying f r1
  => RModify Function f g s l0 r0 l1 r1
  where
  rmodify _ _ _ _ _ = rProxy
