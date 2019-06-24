module Test.Suites.Data.Struct.Set
  ( suites
  ) where

import Prelude (discard, (+), (*), ($), (<<<))

import Data.Struct.Equal (equal)
import Data.Struct.Map (map)
import Data.Struct.Set (set)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Record.Builder (build)
import Record.Builder (insert, rename) as Builder
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "set" do
    test "Builder #0" do
      let s = SProxy :: SProxy "a"
      let value = 0
      let builder = set s value
      build builder { a: 100 } `shouldEqual` { a: value }
    test "Builder #1" do
      let s = SProxy :: SProxy "a"
      let value = "hello"
      let builder = set s value
      build builder { a: 100 } `shouldEqual` { a: value }
    test "Builder #2" do
      let input = { a0: 10, a1: 11, a2: 12, a3: 13 }
      let f =
            Builder.insert (SProxy :: SProxy "a5") 5
              <<< Builder.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
              <<< map { a0: \i -> i * 2, a1: \i -> i * 3 }
              <<< set (SProxy :: SProxy "a1") 0
              <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
      build f input `shouldEqual` { a0: 22, a1: 0, a2: 12, a4: 13, a5: 5 }
    test "Function #0" do
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = set s value
      fn { a: 100 } `shouldEqual` { a: value }
    test "Function #1" do
      let s = SProxy :: SProxy "a"
      let value = "hello"
      let fn = set s value
      fn { a: 100 } `shouldEqual` { a: value }
    test "RProxy #0" do
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = set s value
      let rproxy = RProxy :: RProxy (a :: Int)
      shouldBeTrue $ equal rproxy $ fn rproxy
    test "RProxy #1" do
      let s = SProxy :: SProxy "a"
      let value = "hello"
      let fn = set s value
      let rproxy0 = RProxy :: RProxy (a :: Int)
      let rproxy1 = RProxy :: RProxy (a :: String)
      shouldBeTrue $ equal rproxy1 $ fn rproxy0
    test "Variant #0" do
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = set s value
      let variant0 = inj s 100 :: Variant (a :: Int)
      let variant1 = inj s value :: Variant (a :: Int)
      fn variant0 `shouldEqual` variant1
    test "Variant #1" do
      let s = SProxy :: SProxy "a"
      let value = "hello"
      let fn = set s value
      let variant0 = inj s 100 :: Variant (a :: Int)
      let variant1 = inj s value :: Variant (a :: String)
      fn variant0 `shouldEqual` variant1
    test "Variant #2" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let value = 0
      let fn = set s1 value
      let variant0 = inj s0 100 :: Variant (a0 :: Int, a1 :: Int)
      let variant1 = inj s1 value :: Variant (a0 :: Int, a1 :: Int)
      fn variant0 `shouldEqual` variant1
    test "Variant #3" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let value = "hello"
      let fn = set s1 value
      let variant0 = inj s0 100 :: Variant (a0 :: Int, a1 :: Int)
      let variant1 = inj s1 value :: Variant (a0 :: Int, a1 :: String)
      fn variant0 `shouldEqual` variant1
