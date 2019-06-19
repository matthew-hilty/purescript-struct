module Data.Struct.RDelete
  ( class RDelete
  , rdelete
  ) where

import Data.Symbol (class IsSymbol, SProxy)
import Record (delete) as Record
import Record.Builder (Builder)
import Record.Builder (delete) as Builder
import Type.Row (class Cons, class Lacks, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy) as TypeRow

class RDelete
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
  rdelete
    :: forall v
     . Cons s v r1 r0
    => Lacks s r1
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
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

instance rdeleteRProxy
  :: RDelete Function RProxy g s l0 r0 l1 r1
  where
  rdelete _ _ _ _ = RProxy
