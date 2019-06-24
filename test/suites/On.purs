module Test.Suites.Data.Struct.On.On
  ( suites
  ) where

import Prelude (discard, show, (#), (<>))

import Data.Struct.On (on)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, case_, default, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

s0 = SProxy :: SProxy "a0"
s1 = SProxy :: SProxy "a1"
s2 = SProxy :: SProxy "a2"

suites :: TestSuite
suites =
  suite "on" do
    suite "Variant" do

      suite "All cases explicitly managed" do
        let
          fn :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean) -> String
          fn = case_
            # on s0 (\x -> "a0: " <> show x)
            # on s1 (\x -> "a1: " <> x)
            # on s2 (\x -> "a2: " <> show x)
        test "#0" do
          let x = inj s0 0 :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          fn x `shouldEqual` "a0: 0"
        test "#1" do
          let x = inj s1 "1" :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          fn x `shouldEqual` "a1: 1"
        test "#2" do
          let x = inj s2 false :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          fn x `shouldEqual` "a2: false"

      suite "Default for unmanaged cases" do
        let defaultMessage = "no match"
        let
          fn :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean) -> String
          fn = default defaultMessage
            # on s1 (\x -> "a1: " <> x)
        test "#0" do
          let x = inj s0 0 :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          fn x `shouldEqual` defaultMessage
        test "#1" do
          let x = inj s1 "1" :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          fn x `shouldEqual` "a1: 1"
        test "#2" do
          let x = inj s2 false :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          fn x `shouldEqual` defaultMessage
