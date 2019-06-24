module Test.Suites.Data.Struct.Const.Const
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Const (const)
import Data.Struct.Equal (equal)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Record.Builder (build)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "const" do
    test "Builder" do
      let value0 = { a: 0 }
      let value1 = { b: "b" }
      let builder = const value0
      build builder value1 `shouldEqual` value0

    test "Function" do
      let value0 = { a: 0 }
      let value1 = { b: "b" }
      let fn = const value0
      fn value1 `shouldEqual` value0

    test "RProxy" do
      let value0 = RProxy :: RProxy (a :: Int)
      let value1 = RProxy :: RProxy (b :: Int)
      let fn = const value0
      shouldBeTrue $ equal value0 $ fn value1

    test "Variant" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "b0"
      let value0 = inj s0 0 :: Variant (a0 :: Int, a1 :: Int)
      let value1 = inj s1 "hello" :: Variant (b0 :: String, b1 :: String)
      let fn = const value0
      fn value1 `shouldEqual` value0
