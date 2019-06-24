module Test.Suites.Data.Struct.Delete.Delete
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Delete (delete)
import Data.Struct.Equal (equal)
import Data.Symbol (SProxy(SProxy))
import Record.Builder (build)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "delete" do
    suite "Builder" do
      test "#0" do
        let value = { a0: 0 }
        let builder = delete (SProxy :: SProxy "a0")
        build builder value `shouldEqual` {}
      test "#1" do
        let value = { a0: 0, a1: 1 }
        let builder = delete (SProxy :: SProxy "a1")
        build builder value `shouldEqual` { a0: value.a0 }
    suite "Function" do
      test "#0" do
        let value = { a0: 0 }
        let fn = delete (SProxy :: SProxy "a0")
        fn value `shouldEqual` {}
      test "#1" do
        let value = { a0: 0, a1: 1 }
        let fn = delete (SProxy :: SProxy "a1")
        fn value `shouldEqual` { a0: value.a0 }
    suite "RProxy" do
      test "#0" do
        let value0 = RProxy :: RProxy (a :: Int)
        let value1 = RProxy :: RProxy ()
        let fn = delete (SProxy :: SProxy "a")
        shouldBeTrue $ equal value1 $ fn value0
      test "#1" do
        let value0 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let value1 = RProxy :: RProxy (a0 :: Int)
        let fn = delete (SProxy :: SProxy "a1")
        shouldBeTrue $ equal value1 $ fn value0
