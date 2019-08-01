module Test.Suites.Data.Struct.RDelete
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Delete (rdelete)
import Data.Struct.Equal (requal)
import Data.Symbol (SProxy(SProxy))
import Record.Builder (build)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "rdelete" do
    suite "Builder" do
      test "#0" do
        let value = { a0: 0 }
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy Nil
        let builder = rdelete l0 l1 (SProxy :: SProxy "a0")
        build builder value `shouldEqual` {}
      test "#1" do
        let value = { a0: 0, a1: 1 }
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let builder = rdelete l0 l1 (SProxy :: SProxy "a1")
        build builder value `shouldEqual` { a0: value.a0 }
    suite "Function" do
      test "#0" do
        let value = { a0: 0 }
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy Nil
        let fn = rdelete l0 l1 (SProxy :: SProxy "a0")
        fn value `shouldEqual` {}
      test "#1" do
        let value = { a0: 0, a1: 1 }
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let fn = rdelete l0 l1 (SProxy :: SProxy "a1")
        fn value `shouldEqual` { a0: value.a0 }
    suite "RProxy" do
      test "#0" do
        let value0 = RProxy :: RProxy (a :: Int)
        let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let value1 = RProxy :: RProxy ()
        let l1 = RLProxy :: RLProxy Nil
        let fn = rdelete l0 l1 (SProxy :: SProxy "a")
        shouldBeTrue $ requal l1 value1 $ fn value0
      test "#1" do
        let value0 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RProxy :: RProxy (a0 :: Int)
        let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let fn = rdelete l0 l1 (SProxy :: SProxy "a1")
        shouldBeTrue $ requal l1 value1 $ fn value0
