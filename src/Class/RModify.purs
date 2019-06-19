module Data.Struct.RModify
  ( class RModify
  , rmodify
  ) where

import Prelude (identity, (<<<))

import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Record (modify) as Record
import Record.Builder (Builder)
import Record.Builder (modify) as Builder
import Type.Row (class Cons, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy) as TypeRow
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
    :: forall r v0 v1
     . Cons s v0 r r0
    => Cons s v1 r r1
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> g s
    -> (v0 -> v1)
    -> p (f r0) (f r1)

instance rmodifyBuilder
  :: IsSymbol s
  => RModify Builder Record SProxy s l0 r0 l1 r1
  where
  rmodify _ _ = Builder.modify

instance rmodifyRecord
  :: IsSymbol s
  => RModify Function Record SProxy s l0 r0 l1 r1
  where
  rmodify _ _ = Record.modify

instance rmodifyRProxy :: RModify Function RProxy g s l0 r0 l1 r1 where
  rmodify _ _ _ _ _ = RProxy

instance rmodifyVariant
  ::  IsSymbol s
  => RModify Function Variant SProxy s l0 r0 l1 r1
  where
  rmodify _ _ s f =
    Variant.on
      s
      (Variant.inj s <<< f)
      (unsafeCoerce <<< identity)
