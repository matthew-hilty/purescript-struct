module Test.Suites.Data.Struct.Insert.Insert
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Equal (equal)
import Data.Struct.Insert (insert)
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
  suite "insert" do
    test "Builder" do
      let s = SProxy :: SProxy "a"
      let value = 0
      let builder = insert s value
      build builder {} `shouldEqual` { a: value }
    test "Function" do
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = insert s value
      fn {} `shouldEqual` { a: value }
    test "RProxy" do
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = insert s value
      let rproxy0 = RProxy :: RProxy ()
      let rproxy1 = RProxy :: RProxy (a :: Int)
      shouldBeTrue $ equal rproxy1 $ fn rproxy0
    test "Variant" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let v0 = 0
      let v1 = 1
      let variant0 = inj s0 v0 :: Variant (a0 :: Int)
      let variant1 = inj s1 v1 :: Variant (a0 :: Int, a1 :: Int)
      let fn = insert s1 v1
      fn variant0 `shouldEqual` variant1
