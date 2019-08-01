module Test.Suites.Data.Struct.RContractOrAlt
  ( suites
  ) where

import Prelude (discard, identity, pure, unit, ($), (>>>), (>=>))

import Data.Maybe (Maybe(Just, Nothing))
import Data.Struct.ContractOrAlt (rcontractOrAlt)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

suites :: TestSuite
suites =
  suite "rcontractOrAlt" do
    suite "Maybe" do
      suite "Record" do
        test "#0" do
          let l = RLProxy :: RLProxy Nil
          rcontractOrAlt l l {} `shouldEqual` Just {}
        test "#1" do
          let l0 = RLProxy :: RLProxy (Cons "a" Int Nil)
          let l1 = RLProxy :: RLProxy Nil
          rcontractOrAlt l0 l1 { a: 0 } `shouldEqual` Just {}
        test "#2" do
          let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let l1 = RLProxy :: RLProxy Nil
          rcontractOrAlt l0 l1 { a0: 0, a1: 1 } `shouldEqual` Just {}
        test "#3" do
          let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let value = { a0: 0, a1: 1 }
          rcontractOrAlt l0 l1 value `shouldEqual` Just { a0: value.a0 }
        test "#4" do
          let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let l1 = RLProxy :: RLProxy (Cons "a1" Int Nil)
          let value = { a0: 0, a1: 1 }
          rcontractOrAlt l0 l1 value `shouldEqual` Just { a1: value.a1 }
        test "#5" do
          let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
          let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let l2 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let l3 = RLProxy :: RLProxy Nil
          let value = { a0: 0, a1: 1, a2: 2 }
          let fn = rcontractOrAlt l0 l1 >=> rcontractOrAlt l1 l2 >=> rcontractOrAlt l2 l3
          fn value `shouldEqual` Just {}
        test "#6" do
          let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
          let l1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let l2 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let l3 = RLProxy :: RLProxy Nil
          let value = { a0: 0, a1: 1, a2: 2 }
          let fn =
                    rcontractOrAlt l0 l1
                      >=> rcontractOrAlt l1 l2
                      >>> identity
                      >=> rcontractOrAlt l2 l3
          fn value `shouldEqual` Just {}
        test "#7 -- Should Not Compile" $ pure unit
  --         let l0 = RLProxy :: RLProxy Nil
  --         let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
  --         rcontractOrAlt l0 l1 {} `shouldEqual` Just { a0: 0 }
        test "#8 -- Reorder" do
          let
            l :: RLProxy
                  (Cons "a0" Int
                  (Cons "a1" Int
                  (Cons "a2" Int
                  (Cons "a3" Int
                  (Cons "a4" Int
                  (Cons "a5" Int Nil))))))
            l = RLProxy
          let value = { a0: 0, a1: 1, a2: 2, a3: 3, a4: 4, a5: 5 }
          rcontractOrAlt l l value
            `shouldEqual`
            Just
              { a5: value.a5
              , a3: value.a3
              , a1: value.a1
              , a0: value.a0
              , a2: value.a2
              , a4: value.a4
              }

      suite "Variant" do
        test "#0" do
          let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let l1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let s = SProxy :: SProxy "a0"
          let value = 0
          let variant = inj s value :: Variant (a0 :: Int, a1 :: Int)
          let result = (Just $ inj s value) :: Maybe (Variant (a0 :: Int))
          rcontractOrAlt l0 l1 variant `shouldEqual` result
        test "#1" do
          let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let l1 = RLProxy :: RLProxy (Cons "a1" Int Nil)
          let s = SProxy :: SProxy "a0"
          let variant = inj s 0 :: Variant (a0 :: Int, a1 :: Int)
          let result = Nothing :: Maybe (Variant (a1 :: Int))
          rcontractOrAlt l0 l1 variant `shouldEqual` result
        test "#2" do
          let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let l1 = RLProxy :: RLProxy Nil
          let s = SProxy :: SProxy "a0"
          let variant = inj s 0 :: Variant (a0 :: Int)
          let result = Nothing :: Maybe (Variant ())
          rcontractOrAlt l0 l1 variant `shouldEqual` result
    suite "Array" do
      suite "Variant" do
        test "#0" do
          let l0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let l1 = RLProxy :: RLProxy (Cons "a1" Int Nil)
          let s = SProxy :: SProxy "a0"
          let variant = inj s 0 :: Variant (a0 :: Int, a1 :: Int)
          let result = [] :: Array (Variant (a1 :: Int))
          rcontractOrAlt l0 l1 variant `shouldEqual` result
