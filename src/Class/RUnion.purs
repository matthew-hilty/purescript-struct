module Data.Struct.RUnion
  ( class RUnion
  , runion
  ) where

import Record (union) as Record
import Record.Builder (Builder)
import Record.Builder (union) as Builder
import Type.Row (class Union , RProxy(RProxy) , kind RowList)
import Type.Row (RLProxy) as TypeRow

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
    :: Union r0 r1 r2
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> f r0
    -> p (f r1) (f r2)

instance runionBuilder
  :: Union r1 r0 r2
  => RUnion Builder Record l0 r0 l1 r1 l2 r2
  where
  runion _ _ _ = Builder.union

instance runionRecord :: RUnion Function Record l0 r0 l1 r1 l2 r2 where
  runion _ _ _ = Record.union

instance runionRProxy :: RUnion Function RProxy l0 r0 l1 r1 l2 r2 where
  runion _ _ _ _ _ = RProxy
