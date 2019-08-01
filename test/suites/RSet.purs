module Test.Suites.Data.Struct.RSet
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Equal (requal)
import Data.Struct.Set (rset)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Record.Builder (build)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "rset" do
    test "Builder #0" do
      let l = RLProxy :: RLProxy (Cons "a" Int Nil)
      let s = SProxy :: SProxy "a"
      let value = 0
      let builder = rset l l s value
      build builder { a: 100 } `shouldEqual` { a: value }
    test "Builder #1" do
      let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a" String Nil)
      let s = SProxy :: SProxy "a"
      let value = "hello"
      let builder = rset l0 l1 s value
      build builder { a: 100 } `shouldEqual` { a: value }
    test "Function #0" do
      let l = RLProxy :: RLProxy (Cons "a" Int Nil)
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = rset l l s value
      fn { a: 100 } `shouldEqual` { a: value }
    test "Function #1" do
      let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a" String Nil)
      let s = SProxy :: SProxy "a"
      let value = "hello"
      let fn = rset l0 l1 s value
      fn { a: 100 } `shouldEqual` { a: value }
    test "RProxy #0" do
      let l = RLProxy :: RLProxy (Cons "a" Int Nil)
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = rset l l s value
      let rproxy = RProxy :: RProxy (a :: Int)
      shouldBeTrue $ requal l rproxy $ fn rproxy
    test "RProxy #1" do
      let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a" String Nil)
      let s = SProxy :: SProxy "a"
      let value = "hello"
      let fn = rset l0 l1 s value
      let rproxy0 = RProxy :: RProxy (a :: Int)
      let rproxy1 = RProxy :: RProxy (a :: String)
      shouldBeTrue $ requal l1 rproxy1 $ fn rproxy0
    test "Variant #0" do
      let l = RLProxy :: RLProxy (Cons "a" Int Nil)
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = rset l l s value
      let variant0 = inj s 100 :: Variant (a :: Int)
      let variant1 = inj s value :: Variant (a :: Int)
      fn variant0 `shouldEqual` variant1
    test "Variant #1" do
      let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a" String Nil)
      let s = SProxy :: SProxy "a"
      let value = "hello"
      let fn = rset l0 l1 s value
      let variant0 = inj s 100 :: Variant (a :: Int)
      let variant1 = inj s value :: Variant (a :: String)
      fn variant0 `shouldEqual` variant1
    test "Variant #2" do
      let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let value = 0
      let fn = rset l l s1 value
      let variant0 = inj s0 100 :: Variant (a0 :: Int, a1 :: Int)
      let variant1 = inj s1 value :: Variant (a0 :: Int, a1 :: Int)
      fn variant0 `shouldEqual` variant1
    test "Variant #3" do
      let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
      let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let value = "hello"
      let fn = rset l0 l1 s1 value
      let variant0 = inj s0 100 :: Variant (a0 :: Int, a1 :: Int)
      let variant1 = inj s1 value :: Variant (a0 :: Int, a1 :: String)
      fn variant0 `shouldEqual` variant1
