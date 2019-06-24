module Test.Suites.Data.Struct.Union.Union
  ( suites
  ) where

import Prelude (discard, pure, unit, ($), (<<<))

import Data.Struct.Equal (equal)
import Data.Struct.Nub (nub)
import Data.Struct.Union (union)
import Record.Builder (build)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "union" do
    suite "Builder" do
      test "#0" do
        let builder = union {}
        build builder {} `shouldEqual` {}
      test "#1" do
        let builder = union {}
        let value = { a: 0 }
        build builder value `shouldEqual` value
      test "#2" do
        let value0 = { a0: 0 }
        let builder = union value0
        let value1 = { a1: "1" }
        build builder value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#3 -- Should Not Compile" $ pure unit
--         let value0 = { a0: 0 }
--         let builder = union value0
--         let value1 = { a0: 1000, a1: "1001" }
--         build builder value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#4 -- nub" do
        let value0 = { a0: 0 }
        let builder = nub <<< union value0
        let value1 = { a0: 1000, a1: "1001" }
        build builder value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#5 -- Should Not Compile" $ pure unit
--         let value0 = { a0: 0, a1: "1" }
--         let builder = union value0
--         let value1 = { a0: 1000, a1: "1001" }
--         build builder value1 `shouldEqual` { a0: value0.a0, a1: value0.a1 }
      test "#6 -- nub" do
        let value0 = { a0: 0, a1: "1" }
        let builder = nub <<< union value0
        let value1 = { a0: 1000, a1: "1001" }
        build builder value1 `shouldEqual` { a0: value0.a0, a1: value0.a1 }

    suite "Function" do
      test "#0" do
        let fn = union {}
        fn {} `shouldEqual` {}
      test "#1" do
        let fn = union {}
        let value = { a: 0 }
        fn value `shouldEqual` value
      test "#2" do
        let value0 = { a0: 0 }
        let fn = union value0
        let value1 = { a1: "1" }
        fn value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#3 -- Should Not Compile" $ pure unit
--         let value0 = { a0: 0 }
--         let fn = union value0
--         let value1 = { a0: 1000, a1: "1001" }
--         fn value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#4 -- nub" do
        let value0 = { a0: 0 }
        let fn = nub <<< union value0
        let value1 = { a0: 1000, a1: "1001" }
        fn value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#5 -- Should Not Compile" $ pure unit
--         let value0 = { a0: 0, a1: "1" }
--         let fn = union value0
--         let value1 = { a0: 1000, a1: "1001" }
--         fn value1 `shouldEqual` { a0: value0.a0, a1: value0.a1 }
      test "#6 -- nub" do
        let value0 = { a0: 0, a1: "1" }
        let fn = nub <<< union value0
        let value1 = { a0: 1000, a1: "1001" }
        fn value1 `shouldEqual` { a0: value0.a0, a1: value0.a1 }

    suite "RProxy" do
      test "#0" do
        let value = RProxy :: RProxy ()
        let fn = union value
        shouldBeTrue $ equal value $ fn value
      test "#1" do
        let value0 = RProxy :: RProxy ()
        let fn = union value0
        let value1 = RProxy :: RProxy (a0 :: Int)
        shouldBeTrue $ equal value1 $ fn value1
      test "#2" do
        let value0 = RProxy :: RProxy (a0 :: Int)
        let fn = union value0
        let value1 = RProxy :: RProxy (a1 :: String)
        let value2 = RProxy :: RProxy (a0 :: Int, a1 :: String)
        shouldBeTrue $ equal value2 $ fn value1
      test "#3 -- Should Not Compile" $ pure unit
--         let value0 = RProxy :: RProxy (a0 :: Int)
--         let fn = union value0
--         let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
--         shouldBeTrue $ equal value1 $ fn value1
      test "#4" do
        let value0 = RProxy :: RProxy (a0 :: Int)
        let fn = union value0
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let value2 = RProxy :: RProxy (a0 :: Int, a0 :: Int, a1 :: Int)
        shouldBeTrue $ equal value2 $ fn value1
      test "#5 -- nub" do
        let value0 = RProxy :: RProxy (a0 :: Int)
        let fn = nub <<< union value0
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        shouldBeTrue $ equal value1 $ fn value1
      test "#6 -- Should Not Compile" $ pure unit
--         let value = RProxy :: RProxy (a0 :: Int, a1 :: Int)
--         let fn = union value
--         shouldBeTrue $ equal value $ fn value
      test "#7" do
        let value0 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let fn = union value0
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a0 :: Int, a1 :: Int)
        shouldBeTrue $ equal value1 $ fn value0
      test "#8 -- nub" do
        let value = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let fn = nub <<< union value
        shouldBeTrue $ equal value $ fn value
