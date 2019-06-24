module Test.Suites.Data.Struct.DisjointUnion.DisjointUnion
  ( suites
  ) where

import Prelude (discard)

import Data.Struct.DisjointUnion (disjointUnion)
import Record.Builder (build)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

suites :: TestSuite
suites =
  suite "disjointUnion" do
    suite "Builder" do
      test "#0" do
        let value = {}
        let builder = disjointUnion value
        build builder value `shouldEqual` value
      test "#1" do
        let value0 = {}
        let value1 = { a: 0 }
        let builder = disjointUnion value0
        build builder value1 `shouldEqual` value1
      test "#2" do
        let value0 = { a0: 0 }
        let value1 = { a1: 1 }
        let builder = disjointUnion value0
        build builder value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
    suite "Function" do
      test "#0" do
        let value = {}
        let fn = disjointUnion value
        fn value `shouldEqual` value
      test "#1" do
        let value0 = {}
        let value1 = { a: 0 }
        let fn = disjointUnion value0
        fn value1 `shouldEqual` value1
      test "#2" do
        let value0 = { a0: 0 }
        let value1 = { a1: 1 }
        let fn = disjointUnion value0
        fn value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
