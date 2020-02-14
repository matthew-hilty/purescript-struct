module Data.Struct.Delete.RDelete
  ( class RDelete
  , rdelete
  ) where

import Data.Symbol (class IsSymbol, SProxy)
import Record (delete) as Record
import Record.Builder (Builder)
import Record.Builder (delete) as Builder
import Type.Proxying (class RProxying, rProxy)
import Type.Row (class Cons, class Lacks)
import Type.RowList (RowList)

class RDelete
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
  rdelete
    :: forall (h :: RowList Type -> Type) v
     . Cons s v r1 r0
    => Lacks s r1
    => h l0
    -> h l1
    -> g s
    -> p (f r0) (f r1)

instance rdeleteBuilder
  :: IsSymbol s
  => RDelete Builder Record SProxy s l0 r0 l1 r1
  where
  rdelete _ _ = Builder.delete

instance rdeleteRecord
  :: IsSymbol s
  => RDelete Function Record SProxy s l0 r0 l1 r1
  where
  rdelete _ _ = Record.delete

else instance rdeleteRProxying
  :: RProxying f r1
  => RDelete Function f g s l0 r0 l1 r1
  where
  rdelete _ _ _ _ = rProxy
