module Test.Suites.Data.Struct.REval
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Const (rconst)
import Data.Struct.Delete (rdelete)
import Data.Struct.Equal (requal)
import Data.Struct.Eval (reval)
import Data.Symbol (SProxy(SProxy))
import Record.Builder (Builder)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "reval" do
    suite "Builder" do
      test "#0" do
        let value = { a0: 0 }
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy Nil
        let
            builder :: Builder { a0 :: Int } {}
            builder = rdelete l0 l1 (SProxy :: SProxy "a0")
        reval l0 l1 builder value `shouldEqual` {}
      test "#1" do
        let value = { a0: 0, a1: 1 }
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let
            builder :: Builder { a0 :: Int, a1 :: Int } { a0 :: Int }
            builder = rdelete l0 l1 (SProxy :: SProxy "a1")
        reval l0 l1 builder value `shouldEqual` { a0: value.a0 }
      test "#2" do
        let value0 = { a: 0 }
        let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let value1 = { b: "b" }
        let l1 = RLProxy :: RLProxy (Cons "b" String Nil)
        let
            builder :: Builder { b :: String } { a :: Int }
            builder = rconst l0 l1 value0
        reval l1 l0 builder value1 `shouldEqual` value0

    suite "Function" do
      test "#0" do
        let value = { a0: 0 }
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy Nil
        let
            fn :: { a0 :: Int } -> {}
            fn = rdelete l0 l1 (SProxy :: SProxy "a0")
        reval l0 l1 fn value `shouldEqual` {}
      test "#1" do
        let value = { a0: 0, a1: 1 }
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let
            fn :: { a0 :: Int, a1 :: Int } -> { a0 :: Int }
            fn = rdelete l0 l1 (SProxy :: SProxy "a1")
        reval l0 l1 fn value `shouldEqual` { a0: value.a0 }
      test "#2" do
        let value0 = { a: 0 }
        let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let value1 = { b: "b" }
        let l1 = RLProxy :: RLProxy (Cons "b" String Nil)
        let
            fn :: { b :: String } -> { a :: Int }
            fn = rconst l0 l1 value0
        reval l1 l0 fn value1 `shouldEqual` value0

    suite "RProxy" do
      test "#0" do
        let value0 = RProxy :: RProxy (a :: Int)
        let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let value1 = RProxy :: RProxy ()
        let l1 = RLProxy :: RLProxy Nil
        let
            fn :: RProxy (a :: Int) -> RProxy ()
            fn = rdelete l0 l1 (SProxy :: SProxy "a")
        shouldBeTrue $ requal l1 value1 $ reval l0 l1 fn value0
      test "#1" do
        let value0 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RProxy :: RProxy (a0 :: Int)
        let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let
            fn :: RProxy (a0 :: Int, a1 :: Int) -> RProxy (a0 :: Int)
            fn = rdelete l0 l1 (SProxy :: SProxy "a1")
        shouldBeTrue $ requal l1 value1 $ reval l0 l1 fn value0
