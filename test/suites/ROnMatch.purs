module Test.Suites.Data.Struct.ROnMatch
  ( suites
  ) where

import Prelude (discard, show, (#), (<>))

import Data.Struct.OnMatch (ronMatch)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, case_, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

l_0_1_2 :: RLProxy (Cons "a2" Boolean (Cons "a1" String (Cons "a0" Int Nil)))
l_0_1_2 = RLProxy

l_0_1 :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
l_0_1 = RLProxy

l_0_1' :: RLProxy
        (Cons "a0" (Int -> String)
        (Cons "a1" (String -> String) Nil))
l_0_1' = RLProxy

l_2 :: RLProxy (Cons "a2" Boolean Nil)
l_2 = RLProxy

l_2' :: RLProxy (Cons "a2" (Boolean -> String) Nil)
l_2' = RLProxy

lnil :: RLProxy Nil
lnil = RLProxy

suites :: TestSuite
suites =
  suite "ronMatch" do
    suite "Variant" do
      let s0 = SProxy :: SProxy "a0"
      let s1 = SProxy :: SProxy "a1"
      let s2 = SProxy :: SProxy "a2"
      let
        fn :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean) -> String
        fn = case_
          # ronMatch l_0_1' l_0_1 l_2 l_0_1_2 { a0: \x -> "a0: " <> show x
                                              , a1: \x -> "a1: " <> x
                                              }
          # ronMatch l_2' l_2 lnil l_2 { a2: \x -> "a2: " <> show x }
      test "#0" do
        let x = inj s0 0 :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
        fn x `shouldEqual` "a0: 0"
      test "#1" do
        let x = inj s1 "1" :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
        fn x `shouldEqual` "a1: 1"
      test "#2" do
        let x = inj s2 false :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
        fn x `shouldEqual` "a2: false"
