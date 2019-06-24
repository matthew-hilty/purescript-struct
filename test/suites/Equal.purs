module Test.Suites.Data.Struct.Equal.Equal
  ( suites
  ) where

import Prelude (discard, pure, unit, ($))

import Data.Struct.Equal (equal)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse)
import Type.Row (RProxy(RProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

shouldBeFalse :: Boolean -> Test
shouldBeFalse = assertFalse "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "equal" do
    suite "Record" do
      suite "Equality" do
        test "#0" do
          shouldBeTrue $ equal {} {}
        test "#1" do
          let value = { a: 0 }
          shouldBeTrue $ equal value value
        test "#2" do
          let value = { a0: 0, a1: "1" }
          shouldBeTrue $ equal value value
      suite "Inequality" do
        test "#0 -- Should Not Compile" $ pure unit
--           shouldBeFalse $ equal {} { a: 0 }
        test "#1" do
          shouldBeFalse $ equal { a: 0 } { a: 1 }
        test "#2 -- Should Not Compile" $ pure unit
--           shouldBeFalse $ equal { a: 0 } { a: "0" }
        test "#3" do
          let value = { a0: 0, a1: "1" }
          shouldBeFalse $ equal { a0: 0, a1: "1" } { a0: 1001, a1: "1" }
        test "#4 -- Should Not Compile" $ pure unit
--           let value = { a0: 0, a1: "1" }
--           shouldBeFalse $ equal { a0: 0, a1: "1" } { a0: 1001 }
    suite "RProxy" do
      suite "Equality" do
        test "#0" do
          let value = RProxy :: RProxy ()
          shouldBeTrue $ equal value value
        test "#1" do
          let value = { a: 0 }
          shouldBeTrue $ equal value value
        test "#2" do
          let value = { a0: 0, a1: "1" }
          shouldBeTrue $ equal value value
