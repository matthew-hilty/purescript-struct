module Data.Struct.Insert.RInsert
  ( class RInsert
  , rinsert
  ) where

import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Record (insert) as Record
import Record.Builder (Builder)
import Record.Builder (insert) as Builder
import Type.Proxying (class RProxying, class SProxying, rProxy)
import Type.Row (class Cons, class Lacks)
import Type.RowList (RowList)

class RInsert
  (p  :: Type -> Type -> Type)
  (f  :: Row Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (l0 :: RowList Type)
  (r0 :: Row Type)
  (l1 :: RowList Type)
  (r1 :: Row Type)
  | l0 -> r0
  , l1 -> r1
  where
  rinsert
    :: forall (h :: RowList Type -> Type) v
     . Cons s v r0 r1
    => Lacks s r0
    => h l0
    -> h l1
    -> g s
    -> v
    -> p (f r0) (f r1)

instance rinsertBuilder
  :: ( IsSymbol s
     , SProxying g s
     )
  => RInsert Builder Record g s l0 r0 l1 r1
  where
  rinsert _ _ _ = Builder.insert (SProxy :: SProxy s)

instance rinsertRecord
  :: ( IsSymbol s
     , SProxying g s
     )
  => RInsert Function Record g s l0 r0 l1 r1
  where
  rinsert _ _ _ = Record.insert (SProxy :: SProxy s)

else instance rinsertVariant
  :: ( IsSymbol s
     , SProxying g s
     )
  => RInsert Function Variant g s l0 r0 l1 r1
  where
  rinsert _ _ _ v _ = Variant.inj (SProxy :: SProxy s) v

else instance rinsertRProxying
  :: RProxying f r1
  => RInsert Function f g s l0 r0 l1 r1
  where
  rinsert _ _ _ _ _ = rProxy
