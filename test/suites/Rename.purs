module Test.Suites.Data.Struct.Rename.Rename
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Equal (equal)
import Data.Struct.Rename (rename)
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
  suite "rename" do
    test "Builder" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let record = { a0: 0 }
      let builder = rename s0 s1
      build builder record `shouldEqual` { a1: record.a0 }
    test "Function" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let record = { a0: 0 }
      let fn = rename s0 s1
      fn record `shouldEqual` { a1: record.a0 }
    test "RProxy" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let rproxy0 = RProxy :: RProxy (a0 :: Int)
      let rproxy1 = RProxy :: RProxy (a1 :: Int)
      let fn = rename s0 s1
      shouldBeTrue $ equal rproxy1 $ fn rproxy0
    test "Variant #0" do
      let sa0 = SProxy :: SProxy "a0"
      let sb0 = SProxy :: SProxy "b0"
      let value = 0
      let variantA = inj sa0 value :: Variant (a0 :: Int, a1 :: String)
      let variantB = inj sb0 value :: Variant (b0 :: Int, a1 :: String)
      let fn = rename sa0 sb0
      fn variantA `shouldEqual` variantB
    test "Variant #1" do
      let sa0 = SProxy :: SProxy "a0"
      let sa1 = SProxy :: SProxy "a1"
      let sb1 = SProxy :: SProxy "b1"
      let value = 0
      let variantA = inj sa0 value :: Variant (a0 :: Int, a1 :: String)
      let variantB = inj sa0 value :: Variant (a0 :: Int, b1 :: String)
      let fn = rename sa1 sb1
      fn variantA `shouldEqual` variantB
