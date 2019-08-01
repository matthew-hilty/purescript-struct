module Test.Suites.Data.Struct.RMap
  ( suites
  ) where

import Prelude (discard, (+))

import Data.String.CodePoints (length)
import Data.Struct.Map (rmap)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

suites :: TestSuite
suites =
  suite "rmap" do
    suite "Variant" do
      test "#0" do
        let l0 = RLProxy :: RLProxy Nil
        let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let s = SProxy :: SProxy "a"
        let variant = inj s 0 :: Variant (a :: Int)
        rmap l0 l1 {} variant `shouldEqual` variant

      test "#1" do
        let l0 = RLProxy :: RLProxy (Cons "a" (Int -> Int) Nil)
        let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let s = SProxy :: SProxy "a"
        let fn i = i + 5
        let value = 0
        let variant0 = inj s value :: Variant (a :: Int)
        let variant1 = inj s (fn value) :: Variant (a :: Int)
        rmap l0 l1 { a: fn } variant0 `shouldEqual` variant1

      test "#2" do
        let l0 = RLProxy :: RLProxy (Cons "a" (String -> Int) Nil)
        let l1 = RLProxy :: RLProxy (Cons "a" String Nil)
        let s = SProxy :: SProxy "a"
        let fn str = length str
        let value = "hello"
        let variant0 = inj s value :: Variant (a :: String)
        let variant1 = inj s (fn value) :: Variant (a :: Int)
        rmap l0 l1 { a: fn } variant0 `shouldEqual` variant1

      test "#3" do
        let l0 = RLProxy :: RLProxy (Cons "a1" (Int -> Int) Nil)
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let s = SProxy :: SProxy "a0"
        let fn i = i + 5
        let variant = inj s 0 :: Variant (a0 :: Int, a1 :: Int)
        rmap l0 l1 { a1: fn } variant `shouldEqual` variant

      test "#4" do
        let l0 = RLProxy :: RLProxy (Cons "a0" (Int -> Int) Nil)
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let s = SProxy :: SProxy "a0"
        let fn i = i + 5
        let value = 0
        let variant0 = inj s value :: Variant (a0 :: Int, a1 :: Int)
        let variant1 = inj s (fn value) :: Variant (a0 :: Int, a1 :: Int)
        rmap l0 l1 { a0: fn } variant0 `shouldEqual` variant1

      test "#5" do
        let l0 = RLProxy :: RLProxy (Cons "a0" (Int -> Int) (Cons "a1" (Int -> Int) Nil))
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let s = SProxy :: SProxy "a0"
        let fn i = i + 5
        let value = 0
        let variant0 = inj s value :: Variant (a0 :: Int, a1 :: Int)
        let variant1 = inj s (fn value) :: Variant (a0 :: Int, a1 :: Int)
        rmap l0 l1 { a0: fn, a1: fn } variant0 `shouldEqual` variant1
