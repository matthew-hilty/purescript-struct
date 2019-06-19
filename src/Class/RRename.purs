module Data.Struct.RRename
  ( class RRename
  , rrename
  ) where

import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Record (rename) as Record
import Record.Builder (Builder)
import Record.Builder (rename) as Builder
import Type.Row (class Cons, class Lacks, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy) as TypeRow
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
    :: forall r v
     . Cons s0 v r r0
    => Cons s1 v r r1
    => Lacks s0 r
    => Lacks s0 r1
    => Lacks s1 r
    => Lacks s1 r0
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> g s0
    -> g s1
    -> p (f r0) (f r1)

instance rrenameBuilder
  :: ( IsSymbol s0
     , IsSymbol s1
     )
  => RRename Builder Record SProxy s0 s1 l0 r0 l1 r1 where
  rrename _ _ = Builder.rename

instance rrenameRecord
  :: ( IsSymbol s0
     , IsSymbol s1
     )
  => RRename Function Record SProxy s0 s1 l0 r0 l1 r1 where
  rrename _ _ = Record.rename

instance rrenameRProxy :: RRename Function RProxy g s0 s1 l0 r0 l1 r1 where
  rrename _ _ _ _ _ = RProxy

instance rrenameVariant
  :: ( IsSymbol s0
     , IsSymbol s1
     )
  => RRename Function Variant SProxy s0 s1 l0 r0 l1 r1 where
  rrename _ _ s0 s1 =
    Variant.on
      s0
      (Variant.inj s1)
      unsafeCoerce
