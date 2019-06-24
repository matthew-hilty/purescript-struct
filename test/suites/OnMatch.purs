module Test.Suites.Data.Struct.OnMatch.OnMatch
  ( suites
  ) where

import Prelude (discard, show, (#), (<>))

import Data.Struct.OnMatch (onMatch)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, case_, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

suites :: TestSuite
suites =
  suite "onMatch" do
    suite "Variant" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let s2 = SProxy :: SProxy "a2"
      let
        fn :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean) -> String
        fn = case_
          # onMatch { a0: \x -> "a0: " <> show x
                    , a1: \x -> "a1: " <> x
                    }
          # onMatch { a2: \x -> "a2: " <> show x }
      test "#0" do
        let x = inj s0 0 :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
        fn x `shouldEqual` "a0: 0"
      test "#1" do
        let x = inj s1 "1" :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
        fn x `shouldEqual` "a1: 1"
      test "#2" do
        let x = inj s2 false :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
        fn x `shouldEqual` "a2: false"
