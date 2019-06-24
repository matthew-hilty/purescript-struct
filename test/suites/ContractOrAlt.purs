module Test.Suites.Data.Struct.ContractOrAlt.ContractOrAlt
  ( suites
  ) where

import Prelude (discard, pure, unit, ($))

import Data.Maybe (Maybe(Just, Nothing))
import Data.Struct.ContractOrAlt (contractOrAlt)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

suites :: TestSuite
suites =
  suite "contractOrAlt" do
    suite "Maybe" do
      suite "Record" do
        test "#0" do
          contractOrAlt {} `shouldEqual` Just {}
        test "#1" do
          contractOrAlt { a: 0 } `shouldEqual` Just {}
        test "#2" do
          contractOrAlt { a0: 0, a1: 1 } `shouldEqual` Just {}
        test "#3" do
          let value = { a0: 0, a1: 1 }
          contractOrAlt value `shouldEqual` Just { a0: value.a0 }
        test "#4" do
          let value = { a0: 0, a1: 1 }
          contractOrAlt value `shouldEqual` Just { a1: value.a1 }
        test "#5 -- Does Not Compile" $ pure unit
--           let value = { a0: 0, a1: 1, a2: 2 }
--           let fn = contractOrAlt >=> contractOrAlt >=> contractOrAlt
--           fn value `shouldEqual` Just {}
        test "#6 -- Does Not Compile" $ pure unit
--           let value = { a0: 0, a1: 1, a2: 2 }
--           let fn =
--                     contractOrAlt
--                       >=> contractOrAlt
--                       >>> identity
--                       >=> contractOrAlt
--           fn value `shouldEqual` Just {}
        test "#7 -- Should Not Compile" $ pure unit
--           contractOrAlt {} `shouldEqual` Just { a0: 0 }
        test "#8 -- Reorder" do
          let value = { a0: 0, a1: 1, a2: 2, a3: 3, a4: 4, a5: 5 }
          contractOrAlt value
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
          let s = SProxy :: SProxy "a0"
          let value = 0
          let variant = inj s value :: Variant (a0 :: Int, a1 :: Int)
          let result = (Just $ inj s value) :: Maybe (Variant (a0 :: Int))
          contractOrAlt variant `shouldEqual` result
        test "#1" do
          let s = SProxy :: SProxy "a0"
          let variant = inj s 0 :: Variant (a0 :: Int, a1 :: Int)
          let result = Nothing :: Maybe (Variant (a1 :: Int))
          contractOrAlt variant `shouldEqual` result
        test "#2" do
          let s = SProxy :: SProxy "a0"
          let variant = inj s 0 :: Variant (a0 :: Int)
          let result = Nothing :: Maybe (Variant ())
          contractOrAlt variant `shouldEqual` result
    suite "Array" do
      suite "Variant" do
        test "#0" do
          let s = SProxy :: SProxy "a0"
          let variant = inj s 0 :: Variant (a0 :: Int, a1 :: Int)
          let result = [] :: Array (Variant (a1 :: Int))
          contractOrAlt variant `shouldEqual` result
