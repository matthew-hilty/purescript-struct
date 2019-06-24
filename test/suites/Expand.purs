module Test.Suites.Data.Struct.Expand.Expand
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Equal (equal)
import Data.Struct.Expand (expand)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "expand" do
    test "Variant" do
      let s = SProxy :: SProxy "a0"
      let value = 0
      let variant = inj s value :: Variant (a0 :: Int)
      let result = inj s value :: Variant (a0 :: Int, a1 :: Int)
      expand variant `shouldEqual` result

    test "RProxy" do
      let value0 = RProxy :: RProxy (a0 :: Int)
      let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
      shouldBeTrue $ equal value1 $ expand value0
