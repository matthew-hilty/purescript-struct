module Data.Struct.RConst
  ( class RConst
  , rconst
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Functor.Parameterized.HasConst
  ( class HasConst
  , const
  )
import Control.Subcategory.Restrictable (class Restrictable)
import Type.Row (kind RowList)
import Type.Row (RLProxy) as TypeRow

class RConst
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rconst
    :: TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> f r0
    -> p (f r1) (f r0)

instance rconstHasConstRestrictable
  :: ( HasConst p (f r0)
     , ObjectOf p (f r0)
     , ObjectOf p (f r1)
     , Restrictable Function p
     )
  => RConst p f l0 r0 l1 r1 where
  rconst _ _ = const
