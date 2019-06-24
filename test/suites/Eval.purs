module Test.Suites.Data.Struct.Eval.Eval
  ( suites
  ) where

import Prelude (discard, ($))

import Data.Struct.Const (const)
import Data.Struct.Delete (delete)
import Data.Struct.Equal (equal)
import Data.Struct.Eval (eval)
import Data.Symbol (SProxy(SProxy))
import Record.Builder (Builder)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "eval" do
    suite "Builder" do
      test "#0" do
        let value = { a0: 0 }
        let
            builder :: Builder { a0 :: Int } {}
            builder = delete (SProxy :: SProxy "a0")
        eval builder value `shouldEqual` {}
      test "#1" do
        let value = { a0: 0, a1: 1 }
        let
            builder :: Builder { a0 :: Int, a1 :: Int } { a0 :: Int }
            builder = delete (SProxy :: SProxy "a1")
        eval builder value `shouldEqual` { a0: value.a0 }
      test "#2" do
        let value0 = { a: 0 }
        let value1 = { b: "b" }
        let
            builder :: Builder { b :: String } { a :: Int }
            builder = const value0
        eval builder value1 `shouldEqual` value0

    suite "Function" do
      test "#0" do
        let value = { a0: 0 }
        let
            fn :: { a0 :: Int } -> {}
            fn = delete (SProxy :: SProxy "a0")
        eval fn value `shouldEqual` {}
      test "#1" do
        let value = { a0: 0, a1: 1 }
        let
            fn :: { a0 :: Int, a1 :: Int } -> { a0 :: Int }
            fn = delete (SProxy :: SProxy "a1")
        eval fn value `shouldEqual` { a0: value.a0 }
      test "#2" do
        let value0 = { a: 0 }
        let value1 = { b: "b" }
        let
            fn :: { b :: String } -> { a :: Int }
            fn = const value0
        eval fn value1 `shouldEqual` value0

    suite "RProxy" do
      test "#0" do
        let value0 = RProxy :: RProxy (a :: Int)
        let value1 = RProxy :: RProxy ()
        let
            fn :: RProxy (a :: Int) -> RProxy ()
            fn = delete (SProxy :: SProxy "a")
        shouldBeTrue $ equal value1 $ eval fn value0
      test "#1" do
        let value0 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let value1 = RProxy :: RProxy (a0 :: Int)
        let
            fn :: RProxy (a0 :: Int, a1 :: Int) -> RProxy (a0 :: Int)
            fn = delete (SProxy :: SProxy "a1")
        shouldBeTrue $ equal value1 $ eval fn value0
