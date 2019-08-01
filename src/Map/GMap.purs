module Data.Struct.Map.GMap
  ( class GMap
  , gMap
  )
  where

import Prelude (class Category, class Semigroupoid, identity, (<<<))

import Data.Struct.Get.RGet (class RGet, rget)
import Data.Struct.Modify.RModify (class RModify, rmodify)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Type.Row (class Cons)
import Type.RowList (Cons, Nil, RLProxy(RLProxy), kind RowList)
import Unsafe.Coerce (unsafeCoerce)

class GMap
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  gMap
    :: forall (g :: RowList -> Type)
     . g l0
    -> g l1
    -> Record r0
    -> p (f r1) (f r2)

instance gMapNil :: Category p => GMap p f Nil () l r r where
  gMap _ _ _ = identity

instance gMapCons
  :: ( Cons s (va -> vb) r0' r0
     , Cons s va r r2'
     , Cons s vb r r2
     , GMap p f l0' r0' l1 r1 r2'
     , IsSymbol s
     , RGet Record SProxy s l0 r0
     , RModify p f SProxy s l2' r2' l2 r2
     , Semigroupoid p
     )
  => GMap p f (Cons s v l0') r0 l1 r1 r2
  where
  gMap l0 l1 record0 =
      rmodify x2' x2 s (rget x0 s record0)
        <<< (gMap x0' x1 record0')
    where
    x0' = RLProxy :: RLProxy l0'
    x0  = RLProxy :: RLProxy (Cons s v l0')
    x1  = RLProxy :: RLProxy l1
    x2' = RLProxy :: RLProxy l2'
    x2  = RLProxy :: RLProxy l2
    s   = SProxy  :: SProxy s

    record0' :: Record r0'
    record0' = unsafeCoerce record0
