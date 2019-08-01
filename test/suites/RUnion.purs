module Test.Suites.Data.Struct.RUnion
  ( suites
  ) where

import Prelude (discard, pure, unit, ($), (<<<))

import Data.Struct.Equal (requal)
import Data.Struct.Nub (rnub)
import Data.Struct.Union (runion)
import Record.Builder (build)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "runion" do
    suite "Builder" do
      test "#0" do
        let l = RLProxy :: RLProxy Nil
        let builder = runion l l l {}
        build builder {} `shouldEqual` {}
      test "#1" do
        let l0 = RLProxy :: RLProxy Nil
        let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let builder = runion l0 l1 l1 {}
        let value = { a: 0 }
        build builder value `shouldEqual` value
      test "#2" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy (Cons "a1" String Nil)
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let value0 = { a0: 0 }
        let builder = runion l0 l1 l2 value0
        let value1 = { a1: "1" }
        build builder value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#3 -- Should Not Compile" $ pure unit
--         let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
--         let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
--         let value0 = { a0: 0 }
--         let builder = runion l0 l1 l1 value0
--         let value1 = { a0: 1000, a1: "1001" }
--         build builder value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#4 -- nub" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a0" Int (Cons "a1" String Nil)))
        let value0 = { a0: 0 }
        let builder = rnub l2 l1 <<< runion l0 l1 l2 value0
        let value1 = { a0: 1000, a1: "1001" }
        build builder value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#5 -- Should Not Compile" $ pure unit
--         let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
--         let value0 = { a0: 0, a1: "1" }
--         let builder = runion l l l value0
--         let value1 = { a0: 1000, a1: "1001" }
--         build builder value1 `shouldEqual` { a0: value0.a0, a1: value0.a1 }
      test "#6 -- nub" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let
          l1
            :: RLProxy
                (Cons "a0" Int
                (Cons "a1" String
                (Cons "a0" Int
                (Cons "a1" String Nil))))
          l1 = RLProxy
        let value0 = { a0: 0, a1: "1" }
        let builder = rnub l1 l0 <<< runion l0 l0 l1 value0
        let value1 = { a0: 1000, a1: "1001" }
        build builder value1 `shouldEqual` { a0: value0.a0, a1: value0.a1 }

    suite "Function" do
      test "#0" do
        let l = RLProxy :: RLProxy Nil
        let fn = runion l l l {}
        fn {} `shouldEqual` {}
      test "#1" do
        let l0 = RLProxy :: RLProxy Nil
        let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let fn = runion l0 l1 l1 {}
        let value = { a: 0 }
        fn value `shouldEqual` value
      test "#2" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy (Cons "a1" String Nil)
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let value0 = { a0: 0 }
        let fn = runion l0 l1 l2 value0
        let value1 = { a1: "1" }
        fn value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#3 -- Should Not Compile" $ pure unit
--         let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
--         let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
--         let value0 = { a0: 0 }
--         let fn = runion l0 l1 l1 value0
--         let value1 = { a0: 1000, a1: "1001" }
--         fn value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#4 -- nub" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a0" Int (Cons "a1" String Nil)))
        let value0 = { a0: 0 }
        let fn = rnub l2 l1 <<< runion l0 l1 l2 value0
        let value1 = { a0: 1000, a1: "1001" }
        fn value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
      test "#5 -- Should Not Compile" $ pure unit
--         let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
--         let value0 = { a0: 0, a1: "1" }
--         let fn = runion l l l value0
--         let value1 = { a0: 1000, a1: "1001" }
--         fn value1 `shouldEqual` { a0: value0.a0, a1: value0.a1 }
      test "#6 -- nub" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let
          l1
            :: RLProxy
                (Cons "a0" Int
                (Cons "a1" String
                (Cons "a0" Int
                (Cons "a1" String Nil))))
          l1 = RLProxy
        let value0 = { a0: 0, a1: "1" }
        let fn = rnub l1 l0 <<< runion l0 l0 l1 value0
        let value1 = { a0: 1000, a1: "1001" }
        fn value1 `shouldEqual` { a0: value0.a0, a1: value0.a1 }

    suite "RProxy" do
      test "#0" do
        let l = RLProxy :: RLProxy Nil
        let value = RProxy :: RProxy ()
        let fn = runion l l l value
        shouldBeTrue $ requal l value $ fn value
      test "#1" do
        let l0 = RLProxy :: RLProxy Nil
        let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let value0 = RProxy :: RProxy ()
        let fn = runion l0 l1 l1 value0
        let value1 = RProxy :: RProxy (a0 :: Int)
        shouldBeTrue $ requal l1 value1 $ fn value1
      test "#2" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy (Cons "a1" String Nil)
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let value0 = RProxy :: RProxy (a0 :: Int)
        let fn = runion l0 l1 l2 value0
        let value1 = RProxy :: RProxy (a1 :: String)
        let value2 = RProxy :: RProxy (a0 :: Int, a1 :: String)
        shouldBeTrue $ requal l2 value2 $ fn value1
      test "#3 -- Should Not Compile" $ pure unit
--         let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
--         let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
--         let value0 = RProxy :: RProxy (a0 :: Int)
--         let fn = runion l0 l1 l1 value0
--         let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
--         shouldBeTrue $ requal l1 value1 $ fn value1
      test "#4" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a0" Int (Cons "a1" String Nil)))
        let value0 = RProxy :: RProxy (a0 :: Int)
        let fn = runion l0 l1 l2 value0
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let value2 = RProxy :: RProxy (a0 :: Int, a0 :: Int, a1 :: Int)
        shouldBeTrue $ requal l2 value2 $ fn value1
      test "#5 -- nub" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a0" Int (Cons "a1" String Nil)))
        let value0 = RProxy :: RProxy (a0 :: Int)
        let fn = rnub l2 l1 <<< runion l0 l1 l2 value0
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        shouldBeTrue $ requal l1 value1 $ fn value1
      test "#6 -- Should Not Compile" $ pure unit
--         let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
--         let value = RProxy :: RProxy (a0 :: Int, a1 :: Int)
--         let fn = runion l l l value
--         shouldBeTrue $ requal l value $ fn value
      test "#7" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let
          l1
            :: RLProxy
                (Cons "a0" Int
                (Cons "a1" String
                (Cons "a0" Int
                (Cons "a1" String Nil))))
          l1 = RLProxy
        let value0 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let fn = runion l0 l0 l1 value0
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a0 :: Int, a1 :: Int)
        shouldBeTrue $ requal l1 value1 $ fn value0
      test "#8 -- nub" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let
          l1
            :: RLProxy
                (Cons "a0" Int
                (Cons "a1" String
                (Cons "a0" Int
                (Cons "a1" String Nil))))
          l1 = RLProxy
        let value = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        let fn = rnub l1 l0 <<< runion l0 l0 l1 value
        shouldBeTrue $ requal l1 value $ fn value
