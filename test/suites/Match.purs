module Test.Suites.Data.Struct.Match.Match
  ( suites
  ) where

import Prelude (discard, show, (<>))

import Data.Struct.Match (match)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

l0
  :: RLProxy
      (Cons "a0" (Int -> String)
      (Cons "a1" (String -> String)
      (Cons "a2" (Boolean -> String) Nil)))
l0 = RLProxy

l1
  :: RLProxy
      (Cons "a0" Int
      (Cons "a1" String
      (Cons "a2" Boolean Nil)))
l1 = RLProxy

matches
  :: { a0 :: Int -> String
     , a1 :: String -> String
     , a2 :: Boolean -> String
     }
matches =
  { a0: \a -> "a0: " <> show a
  , a1: \a -> "a1: " <> a
  , a2: \a -> "a2: " <> show a
  }

suites :: TestSuite
suites =
  suite "match" do
    suite "Variant" do
      test "#0" do
        let
          variant :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          variant = inj (SProxy :: SProxy "a0") 0
        match matches variant `shouldEqual` "a0: 0"
      test "#1" do
        let
          variant :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          variant = inj (SProxy :: SProxy "a1") "1"
        match matches variant `shouldEqual` "a1: 1"
      test "#1" do
        let
          variant :: Variant (a0 :: Int, a1 :: String, a2 :: Boolean)
          variant = inj (SProxy :: SProxy "a2") true
        match matches variant `shouldEqual` "a2: true"
