module Test.Suites.Data.Struct.ROn
  ( suites
  ) where

import Prelude (discard, show, (#), (<>))

import Data.Struct.On (ron)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, case_, default, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

s0 = SProxy :: SProxy "a0"
s1 = SProxy :: SProxy "a1"
s2 = SProxy :: SProxy "a2"

l3 :: RLProxy (Cons "a2" Boolean (Cons "a1" String (Cons "a0" Int Nil)))
l3 = RLProxy

suites :: TestSuite
suites =
  suite "ron" do
    suite "Variant" do

      suite "All cases explicitly managed" do
        let l0 = RLProxy :: RLProxy Nil
        let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l2 = RLProxy :: RLProxy (Cons "a1" String (Cons "a0" Int Nil))
        let
          fn :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean) -> String
          fn = case_
            # ron l0 l1 s0 (\x -> "a0: " <> show x)
            # ron l1 l2 s1 (\x -> "a1: " <> x)
            # ron l2 l3 s2 (\x -> "a2: " <> show x)
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
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a2" Boolean Nil))
        let
          fn :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean) -> String
          fn = default defaultMessage
            # ron l2 l3 s1 (\x -> "a1: " <> x)
        test "#0" do
          let x = inj s0 0 :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          fn x `shouldEqual` defaultMessage
        test "#1" do
          let x = inj s1 "1" :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          fn x `shouldEqual` "a1: 1"
        test "#2" do
          let x = inj s2 false :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          fn x `shouldEqual` defaultMessage
