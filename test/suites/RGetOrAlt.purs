module Test.Suites.Data.Struct.RGetOrAlt
  ( suites
  ) where

import Prelude (discard)

import Data.Maybe (Maybe(Just, Nothing))
import Data.Struct.GetOrAlt (rgetOrAlt)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

suites :: TestSuite
suites =
  suite "rgetOrAlt" do
    suite "Variant" do
      suite "Maybe" do
        test "#0" do
          let l = RLProxy :: RLProxy (Cons "a" Int Nil)
          let s = SProxy :: SProxy "a"
          let value = 0
          let variant = inj s value :: Variant (a :: Int)
          rgetOrAlt l s variant `shouldEqual` Just value
        test "#1" do
          let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let s0 = SProxy :: SProxy "a0"
          let s1 = SProxy :: SProxy "a1"
          let variant = inj s0 0 :: Variant (a0 :: Int, a1 :: Int)
          rgetOrAlt l s1 variant `shouldEqual` Nothing
      suite "Array" do
        test "#0" do
          let l = RLProxy :: RLProxy (Cons "a" Int Nil)
          let s = SProxy :: SProxy "a"
          let value = 0
          let variant = inj s value :: Variant (a :: Int)
          rgetOrAlt l s variant `shouldEqual` [value]
        test "#1" do
          let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let s0 = SProxy :: SProxy "a0"
          let s1 = SProxy :: SProxy "a1"
          let variant = inj s0 0 :: Variant (a0 :: Int, a1 :: Int)
          rgetOrAlt l s1 variant `shouldEqual` []
