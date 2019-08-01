module Test.Suites.Data.Struct.RConst
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Const (rconst)
import Data.Struct.Equal (requal)
import Record.Builder (build)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "rconst" do
    test "Builder" do
      let value0 = { a: 0 }
      let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let value1 = { b: "b" }
      let l1 = RLProxy :: RLProxy (Cons "b" String Nil)
      let builder = rconst l0 l1 value0
      build builder value1 `shouldEqual` value0

    test "Function" do
      let value0 = { a: 0 }
      let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let value1 = { b: "b" }
      let l1 = RLProxy :: RLProxy (Cons "b" String Nil)
      let fn = rconst l0 l1 value0
      fn value1 `shouldEqual` value0

    test "RProxy" do
      let value0 = RProxy :: RProxy (a :: Int)
      let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
      let value1 = RProxy :: RProxy (b :: Int)
      let l1 = RLProxy :: RLProxy (Cons "b" String Nil)
      let fn = rconst l0 l1 value0
      shouldBeTrue $ requal l0 value0 $ fn value1
