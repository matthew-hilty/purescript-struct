module Data.Struct.Union.RUnion
  ( class RUnion
  , runion
  ) where

import Control.Subcategory.Restrictable (restrict)
import Record (union) as Record
import Record.Builder (Builder, build)
import Record.Builder (union) as Builder
import Type.Proxying (class RProxying, rProxy)
import Type.Row (class Union)
import Type.RowList (kind RowList)

class RUnion
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  where
  runion
    :: forall (g :: RowList -> Type)
     . Union r0 r1 r2
    => g l0
    -> g l1
    -> g l2
    -> f r0
    -> p (f r1) (f r2)

instance runionBuilder
  :: Union r1 r0 r2
  => RUnion Builder Record l0 r0 l1 r1 l2 r2
  where
  runion _ _ _ record0 =
    -- Whereas the first argument of `Record.union` has precedence,
    -- the second argument of `Builder.union` has precedence.
    restrict \record1 ->
      build (Builder.union record1) record0

else instance runionRecord :: RUnion Function Record l0 r0 l1 r1 l2 r2 where
  runion _ _ _ = Record.union

else instance runionRProxy
  :: RProxying f r2
  => RUnion Function f l0 r0 l1 r1 l2 r2
  where
  runion _ _ _ _ _ = rProxy
