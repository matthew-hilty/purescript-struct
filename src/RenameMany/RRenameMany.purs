module Data.Struct.RenameMany.RRenameMany
  ( class RRenameMany
  , rrenameMany
  ) where

import Prelude (($))

import Control.Subcategory.Restrictable (restrict)
import Data.Array (fromFoldable)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Struct.RenameMany.GRenameMany (class GRenameMany)
import Data.Struct.Utils.ReifyKeyAndValueSymbols
  ( class ReifyKeyAndValueSymbols
  , reifyKeyAndValueSymbols
  )
import Data.Tuple (Tuple)
import Record.Builder (Builder)
import Type.Proxying (class RProxying, rProxy)
import Type.RowList (class ListToRow, class RowToList, kind RowList)

class RRenameMany
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  , l0 l1 -> l2
  , l0 l2 -> l1
  where
  rrenameMany
    :: GRenameMany l0 l1 l2
    => g r0
    -> p (f r1) (f r2)

instance rrenameManyBuilder
  :: ( ListToRow l1 r1
     , ReifyKeyAndValueSymbols l0
     , RowToList r0 l0
     , RowToList r2 l2
     )
  => RRenameMany Builder Record g l0 r0 l1 r1 l2 r2 where
  rrenameMany nameChanges =
    restrict $ \record -> runFn2 unsafeRenameManyBuilder nameChanges' record
    where
    nameChanges' = fromFoldable $ reifyKeyAndValueSymbols nameChanges

instance rrenameManyFunction
  :: ( ListToRow l1 r1
     , ReifyKeyAndValueSymbols l0
     , RowToList r0 l0
     , RowToList r2 l2
     )
  => RRenameMany Function Record g l0 r0 l1 r1 l2 r2 where
  rrenameMany nameChanges record =
    runFn2 unsafeRenameManyFunction nameChanges' record
    where
    nameChanges' = fromFoldable $ reifyKeyAndValueSymbols nameChanges

else instance rrenameManyRProxying
  :: RProxying f r2
  => RRenameMany Function f g l0 r0 l1 r1 l2 r2
  where
  rrenameMany nameChanges _ = rProxy

foreign import unsafeRenameManyBuilder
  :: forall r0 r1
   . Fn2
        (Array (Tuple String String))
        (Record r0)
        (Record r1)

foreign import unsafeRenameManyFunction
  :: forall r0 r1
   . Fn2
        (Array (Tuple String String))
        (Record r0)
        (Record r1)
