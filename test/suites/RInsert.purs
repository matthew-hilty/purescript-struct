module Test.Suites.Data.Struct.RInsert
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Equal (requal)
import Data.Struct.Insert (rinsert)
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
  suite "rinsert" do
    test "Builder" do
      let l0 = RLProxy :: RLProxy Nil
      let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let s = SProxy :: SProxy "a"
      let value = 0
      let builder = rinsert l0 l1 s value
      build builder {} `shouldEqual` { a: value }
    test "Function" do
      let l0 = RLProxy :: RLProxy Nil
      let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = rinsert l0 l1 s value
      fn {} `shouldEqual` { a: value }
    test "RProxy" do
      let l0 = RLProxy :: RLProxy Nil
      let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let s = SProxy :: SProxy "a"
      let value = 0
      let fn = rinsert l0 l1 s value
      let rproxy0 = RProxy :: RProxy ()
      let rproxy1 = RProxy :: RProxy (a :: Int)
      shouldBeTrue $ requal l1 rproxy1 $ fn rproxy0
    test "Variant" do
      let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let v0 = 0
      let v1 = 1
      let variant0 = inj s0 v0 :: Variant (a0 :: Int)
      let variant1 = inj s1 v1 :: Variant (a0 :: Int, a1 :: Int)
      let fn = rinsert l0 l1 s1 v1
      fn variant0 `shouldEqual` variant1
