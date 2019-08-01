module Test.Suites.Data.Struct.RRename
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Equal (requal)
import Data.Struct.Rename (rrename)
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
  suite "rrename" do
    test "Builder" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a1" Int Nil)
      let record = { a0: 0 }
      let builder = rrename l0 l1 s0 s1
      build builder record `shouldEqual` { a1: record.a0 }
    test "Function" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a1" Int Nil)
      let record = { a0: 0 }
      let fn = rrename l0 l1 s0 s1
      fn record `shouldEqual` { a1: record.a0 }
    test "RProxy" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a1" Int Nil)
      let rproxy0 = RProxy :: RProxy (a0 :: Int)
      let rproxy1 = RProxy :: RProxy (a1 :: Int)
      let fn = rrename l0 l1 s0 s1
      shouldBeTrue $ requal l1 rproxy1 $ fn rproxy0
    test "Variant #0" do
      let sa0 = SProxy :: SProxy "a0"
      let sb0 = SProxy :: SProxy "b0"
      let la = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
      let lb = RLProxy :: RLProxy (Cons "b0" Int (Cons "a1" String Nil))
      let value = 0
      let variantA = inj sa0 value :: Variant (a0 :: Int, a1 :: String)
      let variantB = inj sb0 value :: Variant (b0 :: Int, a1 :: String)
      let fn = rrename la lb sa0 sb0
      fn variantA `shouldEqual` variantB
    test "Variant #1" do
      let sa0 = SProxy :: SProxy "a0"
      let sa1 = SProxy :: SProxy "a1"
      let sb1 = SProxy :: SProxy "b1"
      let la = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
      let lb = RLProxy :: RLProxy (Cons "a0" Int (Cons "b1" String Nil))
      let value = 0
      let variantA = inj sa0 value :: Variant (a0 :: Int, a1 :: String)
      let variantB = inj sa0 value :: Variant (a0 :: Int, b1 :: String)
      let fn = rrename la lb sa1 sb1
      fn variantA `shouldEqual` variantB
