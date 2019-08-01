module Data.Struct.RenameMany.GRenameMany
  ( class GRenameMany
  , class GRenameMany_
  ) where

import Data.Struct.Utils.HasSymbolValue (class HasSymbolValue_)
import Type.Data.Boolean
  ( class And
  , class If
  , class Not
  , False
  , True
  , kind Boolean
  )
import Type.Data.RowList (RLProxy)
import Type.Data.Symbol (class Equals)
import Type.RowList (Cons, Nil, kind RowList)

class GRenameMany_
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  (continue :: Boolean)
  | l0 l1 -> l2
  , l0 l2 -> l1

instance renameMany_False :: GRenameMany_ l0 l1 l2 False

instance renameMany_Nil :: GRenameMany_ Nil l l True

instance renameMany_Cons
  :: ( Equals sb s2 eq
     , Not eq uneq
     , If eq
          (RLProxy (Cons sa v l1_ifEq'))
          (RLProxy l1_ifUneq)
          (RLProxy l1)
     , HasSymbolValue_ l0' s2 has uneq
     , Not has lacks
     , And uneq has uneqAndHas
     , And uneq lacks uneqAndLacks
     , If has
          (RLProxy l1_ifHas)
          (RLProxy (Cons s2 v l1_ifLacks'))
          (RLProxy l1_ifUneq)
     , GRenameMany_ l0'                  l1_ifEq'    l2'             eq
     , GRenameMany_ l0'                  lx          (Cons s2 v l2') uneqAndHas
     , GRenameMany_ (Cons sa (f sb) Nil) l1_ifHas    lx              uneqAndHas
     , GRenameMany_ (Cons sa (f sb) l0') l1_ifLacks' l2'             uneqAndLacks
     )
  => GRenameMany_
        (Cons sa (f sb) l0')
        l1
        (Cons s2 v l2')
        True

class GRenameMany
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  | l0 l1 -> l2
  , l0 l2 -> l1

instance renameManyRenameMany_
  :: GRenameMany_ l0 l1 l2 True
  => GRenameMany l0 l1 l2
