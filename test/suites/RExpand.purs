module Test.Suites.Data.Struct.RExpand
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Expand (rexpand)
import Data.Struct.Equal (requal)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "rexpand" do
    test "Variant" do
      let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
      let s = SProxy :: SProxy "a0"
      let value = 0
      let variant = inj s value :: Variant (a0 :: Int)
      let result = inj s value :: Variant (a0 :: Int, a1 :: Int)
      rexpand l0 l1 variant `shouldEqual` result

    test "RProxy" do
      let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
      let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
      let value0 = RProxy :: RProxy (a0 :: Int)
      let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
      let fn = rexpand l0 l1
      shouldBeTrue $ requal l1 value1 $ fn value0
