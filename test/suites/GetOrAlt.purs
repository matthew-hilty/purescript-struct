module Test.Suites.Data.Struct.GetOrAlt.GetOrAlt
  ( suites
  ) where

import Prelude (discard)

import Data.Maybe (Maybe(Just, Nothing))
import Data.Struct.GetOrAlt (getOrAlt)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

suites :: TestSuite
suites =
  suite "getOrAlt" do
    suite "Variant" do
      suite "Maybe" do
        test "#0" do
          let s = SProxy :: SProxy "a"
          let value = 0
          let variant = inj s value :: Variant (a :: Int)
          getOrAlt s variant `shouldEqual` Just value
        test "#1" do
          let s0 = SProxy :: SProxy "a0"
          let s1 = SProxy :: SProxy "a1"
          let variant = inj s0 0 :: Variant (a0 :: Int, a1 :: Int)
          getOrAlt s1 variant `shouldEqual` Nothing
      suite "Array" do
        test "#0" do
          let s = SProxy :: SProxy "a"
          let value = 0
          let variant = inj s value :: Variant (a :: Int)
          getOrAlt s variant `shouldEqual` [value]
        test "#1" do
          let s0 = SProxy :: SProxy "a0"
          let s1 = SProxy :: SProxy "a1"
          let variant = inj s0 0 :: Variant (a0 :: Int, a1 :: Int)
          getOrAlt s1 variant `shouldEqual` []
