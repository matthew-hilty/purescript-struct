module Test.Main
  ( main
  ) where

import Prelude (Unit, discard)

import Effect (Effect)
import Test.Suites.Data.Struct.Compare.Compare (suites) as Compare
import Test.Suites.Data.Struct.Const.Const (suites) as Const
import Test.Suites.Data.Struct.Contract.Contract (suites) as Contract
import Test.Suites.Data.Struct.Delete.Delete (suites) as Delete
import Test.Suites.Data.Struct.Equal.Equal (suites) as Equal
import Test.Suites.Data.Struct.Eval.Eval (suites) as Eval
import Test.Suites.Data.Struct.Expand.Expand (suites) as Expand
import Test.Suites.Data.Struct.Get.Get (suites) as Get
import Test.Suites.Data.Struct.GetOrAlt.GetOrAlt (suites) as GetOrAlt
import Test.Suites.Data.Struct.Map.Map (suites) as Map
import Test.Suites.Data.Struct.Match.Match (suites) as Match
import Test.Suites.Data.Struct.Merge.Merge (suites) as Merge
import Test.Suites.Data.Struct.Modify.Modify (suites) as Modify
import Test.Suites.Data.Struct.On.On (suites) as On
import Test.Suites.Data.Struct.OnMatch.OnMatch (suites) as OnMatch
import Test.Suites.Data.Struct.RCompare (suites) as RCompare
import Test.Suites.Data.Struct.RConst (suites) as RConst
import Test.Suites.Data.Struct.RContractOrAlt (suites) as RContractOrAlt
import Test.Suites.Data.Struct.RDisjointUnion (suites) as RDisjointUnion
import Test.Suites.Data.Struct.Rename.Rename (suites) as Rename
import Test.Suites.Data.Struct.REqual (suites) as REqual
import Test.Suites.Data.Struct.REval (suites) as REval
import Test.Suites.Data.Struct.RExpand (suites) as RExpand
import Test.Suites.Data.Struct.RGet (suites) as RGet
import Test.Suites.Data.Struct.RGetOrAlt (suites) as RGetOrAlt
import Test.Suites.Data.Struct.RInsert (suites) as RInsert
import Test.Suites.Data.Struct.RMap (suites) as RMap
import Test.Suites.Data.Struct.RMatch (suites) as RMatch
import Test.Suites.Data.Struct.RMerge (suites) as RMerge
import Test.Suites.Data.Struct.RModify (suites) as RModify
import Test.Suites.Data.Struct.ROn (suites) as ROn
import Test.Suites.Data.Struct.ROnMatch (suites) as ROnMatch
import Test.Suites.Data.Struct.RRename (suites) as RRename
import Test.Suites.Data.Struct.RRenameMany (suites) as RRenameMany
import Test.Suites.Data.Struct.RSet (suites) as RSet
import Test.Suites.Data.Struct.RSingleton (suites) as RSingleton
import Test.Suites.Data.Struct.RUnion (suites) as RUnion
import Test.Suites.Data.Struct.Set (suites) as Set
import Test.Suites.Data.Struct.Union.Union (suites) as Union
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
   Compare.suites
   Const.suites
   Contract.suites
   Delete.suites
   Equal.suites
   Eval.suites
   Expand.suites
   Get.suites
   GetOrAlt.suites
   Map.suites
   Match.suites
   Merge.suites
   Modify.suites
   On.suites
   OnMatch.suites
   RCompare.suites
   RConst.suites
   RContractOrAlt.suites
   RDisjointUnion.suites
   Rename.suites
   REqual.suites
   REval.suites
   RExpand.suites
   RGet.suites
   RGetOrAlt.suites
   RInsert.suites
   RMap.suites
   RMatch.suites
   RMerge.suites
   RModify.suites
   ROn.suites
   ROnMatch.suites
   RRename.suites
   RRenameMany.suites
   RSet.suites
   RSingleton.suites
   RUnion.suites
   Set.suites
   Union.suites
