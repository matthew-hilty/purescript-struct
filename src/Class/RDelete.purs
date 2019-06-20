module Data.Struct.RDelete
  ( class RDelete
  , rdelete
  ) where

import Data.Symbol (class IsSymbol, SProxy)
import Record (delete) as Record
import Record.Builder (Builder)
import Record.Builder (delete) as Builder
import Type.Proxying (class RLProxying, class RProxying, rProxy)
import Type.Row (class Cons, class Lacks, kind RowList)

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
    :: forall h v
     . Cons s v r1 r0
    => Lacks s r1
    => RLProxying h l0
    => RLProxying h l1
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
