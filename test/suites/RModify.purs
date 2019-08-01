module Test.Suites.Data.Struct.RModify
  ( suites
  ) where

import Prelude (discard, ($), (+))

import Data.String.CodePoints (length)
import Data.Struct.Equal (requal)
import Data.Struct.Modify (rmodify)
import Data.Symbol (SProxy(SProxy))
import Record.Builder (build)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, shouldEqual)
import Type.Row (RProxy(RProxy))
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

shouldBeTrue :: Boolean -> Test
shouldBeTrue = assert "Unsatisfied expectations"

suites :: TestSuite
suites =
  suite "rmodify" do
    suite "Builder" do
      test "#0" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
        let s = SProxy :: SProxy "a1"
        let f i = i + 5
        let builder = rmodify l l s f
        let value = { a0: 0, a1: 1, a2: 2 }
        build builder value `shouldEqual` value { a1 = f value.a1 }
      test "#1" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String (Cons "a2" Int Nil)))
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
        let s = SProxy :: SProxy "a1"
        let builder = rmodify l0 l1 s length
        let value = { a0: 0, a1: "a1", a2: 2 }
-- Why doesn't the following work?
-- build builder value `shouldEqual` value { a1 = length value.a1 }
        build builder value
          `shouldEqual`
          { a0: value.a0, a1: length value.a1, a2: value.a2 }

    suite "Function" do
      test "#0" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
        let s = SProxy :: SProxy "a1"
        let f i = i + 5
        let fn = rmodify l l s f
        let value = { a0: 0, a1: 1, a2: 2 }
        fn value `shouldEqual` value { a1 = f value.a1 }
      test "#1" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String (Cons "a2" Int Nil)))
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
        let s = SProxy :: SProxy "a1"
        let fn = rmodify l0 l1 s length
        let value = { a0: 0, a1: "a1", a2: 2 }
-- Why doesn't the following work?
-- fn value `shouldEqual` value { a1 = length value.a1 }
        fn value `shouldEqual` { a0: value.a0, a1: length value.a1, a2: value.a2 }

    suite "RProxy" do
      test "#0" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
        let s = SProxy :: SProxy "a1"
        let f i = i + 5
        let fn = rmodify l l s f
        let value = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int)
        shouldBeTrue $ requal l value $ fn value
      test "#1" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String (Cons "a2" Int Nil)))
        let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
        let s = SProxy :: SProxy "a1"
        let fn = rmodify l0 l1 s length
        let value0 = RProxy :: RProxy (a0 :: Int, a1 :: String, a2 :: Int)
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int)
        shouldBeTrue $ requal l1 value1 $ fn value0
