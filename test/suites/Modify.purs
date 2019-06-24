module Test.Suites.Data.Struct.Modify.Modify
  ( suites
  ) where

import Prelude (discard, ($), (+))

import Data.String.CodePoints (length)
import Data.Struct.Equal (equal)
import Data.Struct.Modify (modify)
import Data.Symbol (SProxy(SProxy))
import Record.Builder (build)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "modify" do
    suite "Builder" do
      test "#0" do
        let s = SProxy :: SProxy "a1"
        let f i = i + 5
        let builder = modify s f
        let value = { a0: 0, a1: 1, a2: 2 }
        build builder value `shouldEqual` value { a1 = f value.a1 }
      test "#1" do
        let s = SProxy :: SProxy "a1"
        let builder = modify s length
        let value = { a0: 0, a1: "a1", a2: 2 }
        build builder value
          `shouldEqual`
          { a0: value.a0, a1: length value.a1, a2: value.a2 }

    suite "Function" do
      test "#0" do
        let s = SProxy :: SProxy "a1"
        let f i = i + 5
        let fn = modify s f
        let value = { a0: 0, a1: 1, a2: 2 }
        fn value `shouldEqual` value { a1 = f value.a1 }
      test "#1" do
        let s = SProxy :: SProxy "a1"
        let fn = modify s length
        let value = { a0: 0, a1: "a1", a2: 2 }
        fn value `shouldEqual` { a0: value.a0, a1: length value.a1, a2: value.a2 }

    suite "RProxy" do
      test "#0" do
        let s = SProxy :: SProxy "a1"
        let f i = i + 5
        let fn = modify s f
        let value = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int)
        shouldBeTrue $ equal value $ fn value
      test "#1" do
        let s = SProxy :: SProxy "a1"
        let fn = modify s length
        let value0 = RProxy :: RProxy (a0 :: Int, a1 :: String, a2 :: Int)
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int)
        shouldBeTrue $ equal value1 $ fn value0
