module Data.Struct.Const.RConst
  ( class RConst
  , rconst
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Functor.Parameterized.HasConst
  ( class HasConst
  , const
  )
import Control.Subcategory.Restrictable (class Restrictable)
import Type.RowList (RowList)

class RConst
  (p  :: Type -> Type -> Type)
  (f  :: Row Type -> Type)
  (l0 :: RowList Type)
  (r0 :: Row Type)
  (l1 :: RowList Type)
  (r1 :: Row Type)
  | l0 -> r0
  , l1 -> r1
  where
  rconst
    :: forall (g :: RowList Type -> Type)
     . g l0
    -> g l1
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
