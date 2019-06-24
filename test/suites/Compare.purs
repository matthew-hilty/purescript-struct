module Test.Suites.Data.Struct.Compare.Compare
  ( suites
  ) where

import Prelude (discard, negate, Ordering(EQ, GT, LT))

import Data.Struct.Compare (compare)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

suites :: TestSuite
suites =
  suite "compare" do
    suite "Unmixed" do
      test "#0" do
        compare { a: 0 } { a: 0 } `shouldEqual` EQ
      test "#1" do
        compare { a: 1 } { a: 0 } `shouldEqual` GT
      test "#2" do
        compare { a: 0 } { a: 1 } `shouldEqual` LT
      test "#3" do
        compare { a0: 0, a1: 1 } { a0: 0, a1: 1 } `shouldEqual` EQ
      test "#4" do
        compare { a0: 1, a1: 1 } { a0: 0, a1: 1 } `shouldEqual` GT
      test "#5" do
        compare { a0: 0, a1: 2 } { a0: 0, a1: 1 } `shouldEqual` GT
      test "#6" do
        compare { a0: 0, a1: 2 } { a0: 0, a1: 1 } `shouldEqual` GT
      test "#7" do
        compare { a0: -1, a1: 1 } { a0: 0, a1: 1 } `shouldEqual` LT
      test "#8" do
        compare { a0: 0, a1: 0 } { a0: 0, a1: 1 } `shouldEqual` LT
      test "#9" do
        compare { a0: -1, a1: 0 } { a0: 0, a1: 1 } `shouldEqual` LT
    suite "Mixed -- Ordering depends on first field of RowList representation" do
      test "#0" do
        compare { a0: 0, a1: 2 } { a0: 1, a1: 1 } `shouldEqual` LT
      test "#1" do
        compare { a0: 2, a1: 0 } { a0: 1, a1: 1 } `shouldEqual` GT
      test "#2" do
        compare { a1: 2, a0: 0 } { a0: 1, a1: 1 } `shouldEqual` LT
