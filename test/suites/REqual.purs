module Test.Suites.Data.Struct.REqual
  ( suites
  ) where

import Prelude (discard, pure, unit, ($))

import Data.Struct.Equal (requal)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse)
import Type.Row (RProxy(RProxy))
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

shouldBeFalse :: Boolean -> Test
shouldBeFalse = assertFalse "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "requal" do
    suite "Record" do
      suite "Equality" do
        test "#0" do
          let l = RLProxy :: RLProxy Nil
          shouldBeTrue $ requal l {} {}
        test "#1" do
          let l = RLProxy :: RLProxy (Cons "a" Int Nil)
          let value = { a: 0 }
          shouldBeTrue $ requal l value value
        test "#2" do
          let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
          let value = { a0: 0, a1: "1" }
          shouldBeTrue $ requal l value value
      suite "Inequality" do
        test "#0 -- Should Not Compile" $ pure unit
--           let l = RLProxy :: RLProxy Nil
--           shouldBeFalse $ requal l {} { a: 0 }
        test "#1" do
          let l = RLProxy :: RLProxy (Cons "a" Int Nil)
          shouldBeFalse $ requal l { a: 0 } { a: 1 }
        test "#2 -- Should Not Compile" $ pure unit
--           let l = RLProxy :: RLProxy (Cons "a" Int Nil)
--           shouldBeFalse $ requal l { a: 0 } { a: "0" }
        test "#3" do
          let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
          let value = { a0: 0, a1: "1" }
          shouldBeFalse $ requal l { a0: 0, a1: "1" } { a0: 1001, a1: "1" }
        test "#4 -- Should Not Compile" $ pure unit
--           let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
--           let value = { a0: 0, a1: "1" }
--           shouldBeFalse $ requal l { a0: 0, a1: "1" } { a0: 1001 }
    suite "RProxy" do
      suite "Equality" do
        test "#0" do
          let l = RLProxy :: RLProxy Nil
          let value = RProxy :: RProxy ()
          shouldBeTrue $ requal l value value
        test "#1" do
          let l = RLProxy :: RLProxy (Cons "a" Int Nil)
          let value = { a: 0 }
          shouldBeTrue $ requal l value value
        test "#2" do
          let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
          let value = { a0: 0, a1: "1" }
          shouldBeTrue $ requal l value value
