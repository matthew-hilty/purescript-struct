module Data.Struct.Eval.REval
  ( class REval
  , reval
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Slackable (class Slackable, slacken)
import Type.RowList (RowList)

class REval
  (p  :: Type -> Type -> Type)
  (f  :: Row Type -> Type)
  (l0 :: RowList Type)
  (r0 :: Row Type)
  (l1 :: RowList Type)
  (r1 :: Row Type)
  | l0 -> r0
  , l1 -> r1
  where
  reval
    :: forall (g :: RowList Type -> Type)
     . g l0
    -> g l1
    -> p (f r0) (f r1)
    -> f r0
    -> f r1

instance revalSlackable
  :: ( ObjectOf p (f r0)
     , ObjectOf p (f r1)
     , Slackable p
     )
  => REval p f l0 r0 l1 r1
  where
  reval _ _ = slacken
