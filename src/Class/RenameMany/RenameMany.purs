module Data.Struct.RenameMany.RenameMany
  ( class RenameMany
  , class RenameMany_
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
import Type.Data.RowList (RLProxy) -- Argonaut dependency
import Type.Data.Symbol (class Equals)
import Type.Row (Cons, Nil, kind RowList)

class RenameMany_
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  (continue :: Boolean)
  | l0 l1 -> l2
  , l0 l2 -> l1

instance renameMany_False :: RenameMany_ l0 l1 l2 False

instance renameMany_Nil :: RenameMany_ Nil l l True

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
     , RenameMany_ l0'                  l1_ifEq'    l2'             eq
     , RenameMany_ l0'                  lx          (Cons s2 v l2') uneqAndHas
     , RenameMany_ (Cons sa (f sb) Nil) l1_ifHas    lx              uneqAndHas
     , RenameMany_ (Cons sa (f sb) l0') l1_ifLacks' l2'             uneqAndLacks
     )
  => RenameMany_
        (Cons sa (f sb) l0')
        l1
        (Cons s2 v l2')
        True

class RenameMany
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  | l0 l1 -> l2
  , l0 l2 -> l1

instance renameManyRenameMany_
  :: RenameMany_ l0 l1 l2 True
  => RenameMany l0 l1 l2
