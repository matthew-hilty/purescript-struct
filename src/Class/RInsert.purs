module Data.Struct.RInsert
  ( class RInsert
  , rinsert
  ) where

import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Record (insert) as Record
import Record.Builder (Builder)
import Record.Builder (insert) as Builder
import Type.Row (class Cons, class Lacks, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy) as TypeRow

class RInsert
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
  rinsert
    :: forall v
     . Cons s v r0 r1
    => Lacks s r0
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> g s
    -> v
    -> p (f r0) (f r1)

instance rinsertBuilder
  :: IsSymbol s
  => RInsert Builder Record SProxy s l0 r0 l1 r1
  where
  rinsert _ _ = Builder.insert

instance rinsertRecord
  :: IsSymbol s
  => RInsert Function Record SProxy s l0 r0 l1 r1
  where
  rinsert _ _ = Record.insert

instance rinsertRProxy
  :: RInsert Function RProxy g s l0 r0 l1 r1
  where
  rinsert _ _ _ _ _ = RProxy

instance rinsertVariant
  :: IsSymbol s
  => RInsert Function Variant SProxy s l0 r0 l1 r1
  where
  rinsert _ _ s v _ = Variant.inj s v
