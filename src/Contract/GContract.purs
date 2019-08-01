module Data.Struct.Contract.GContract
  ( class GContract
  , gContract
  ) where

import Prelude (class Category, class Semigroupoid, identity, (<<<))

import Data.Struct.Const.RConst (class RConst, rconst)
import Data.Struct.Delete.RDelete (class RDelete, rdelete)
import Data.Struct.Empty.REmpty (class REmpty, rempty)
import Data.Symbol (SProxy(SProxy))
import Type.Row (class Cons, class Lacks)
import Type.RowList (Cons, Nil, RLProxy(RLProxy), kind RowList)


class GContract
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
  , l0 l1 -> l2
  where
  gContract
    :: forall (g :: RowList -> Type)
     . g l0
    -> g l1
    -> g l2
    -> p (f r1) (f r2)

instance gContract_Nil_Nil_Nil
  :: Category p
  => GContract p f Nil () Nil () Nil ()
  where
  gContract _ _ _ = identity

instance gContract_Nil_Cons_Cons
  :: Category p
  => GContract p f Nil () (Cons s1 v1 l1') unifyR (Cons s2 v2 l2') unifyR
  where
  gContract _ _ _ = identity

instance gContract_Cons_Cons_Nil
  :: ( RConst p f Nil () (Cons s v l') r
     , REmpty f
     )
  => GContract p f (Cons s v l') r (Cons s v l') r Nil ()
  where
  gContract _ l nil = rconst nil l rempty

else instance gContract_Cons_Cons_Cons
  :: ( Cons s0 v0 r2' r2
     , GContract
          p
          f
          l0'
          r0'
          (Cons s1 v1 l1')
          r1
          (Cons s0 v0 (Cons s2' v2' l2''))
          r2
     , Lacks s0 r2'
     , RDelete
          p
          f
          SProxy
          s0
          (Cons s0 v0 (Cons s2' v2' l2''))
          r2
          (Cons s2' v2' l2'')
          r2'
     , Semigroupoid p
     )
  => GContract
        p
        f
        (Cons s0 v0 l0')
        r0
        (Cons s1 v1 l1')
        r1
        (Cons s2' v2' l2'')
        r2'
  where
  gContract l0 l1 l2' =
      rdelete x2 x2' s0 <<< gContract x0' x1 x2
    where
    x0' = RLProxy :: RLProxy l0'
    x1  = RLProxy :: RLProxy (Cons s1 v1 l1')
    x2' = RLProxy :: RLProxy (Cons s2' v2' l2'')
    x2  = RLProxy :: RLProxy (Cons s0 v0 (Cons s2' v2' l2''))
    s0  = SProxy  :: SProxy s0
