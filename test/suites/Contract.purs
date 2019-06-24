module Test.Suites.Data.Struct.Contract.Contract
  ( suites
  ) where

import Prelude (discard, pure, unit, ($))

import Data.Struct.Contract (contract)
import Record.Builder (build)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

suites :: TestSuite
suites =
  suite "contract" do
    suite "Builder" do
      test "#0" do
        let input = {}
        build contract input `shouldEqual` {}

      test "#1" do
        let input = { a0: 0 }
        build contract input `shouldEqual` {}

      test "#2" do
        let input = { a0: 0 }
        build contract input `shouldEqual` input

      test "#3" do
        let input = { a0: 0, a1: 1, a2: 2 }
        build contract input `shouldEqual` { a1: 1, a2: 2 }

      test "#4" do
        let input = { a0: 0, a1: 1, a2: 2 }
        build contract input `shouldEqual` { a0: 0, a2: 2 }

      test "#5 -- Should Not Compile" $ pure unit
--         let input = { a0: 0, a1: 1, a2: 2 }
--         build (contract <<< contract) input
--           `shouldEqual`
--           { a0: 0, a2: 2 }

      test "#6 -- Should Not Compile" $ pure unit
--         let input = { a0: 0, a1: 1, a2: 2 }
--         build (contract <<< identity <<< contract) input
--           `shouldEqual`
--           { a0: 0, a2: 2 }

      test "#7 -- Should Not Compile" $ pure unit
--         let input = {}
--         build contract input `shouldEqual` { a0: 0 }

      test "#8" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3, a4: 4, a5: 5 }
        build contract input
          `shouldEqual`
          { a5: 5, a3: 3, a1: 1, a0: 0, a2: 2, a4: 4 }

    suite "Function" do
      test "#0" do
        let input = {}
        contract input `shouldEqual` {}

      test "#1" do
        let input = { a0: 0 }
        contract input `shouldEqual` {}

      test "#2" do
        let input = { a0: 0 }
        contract input `shouldEqual` input

      test "#3" do
        let input = { a0: 0, a1: 1, a2: 2 }
        contract input `shouldEqual` { a1: 1, a2: 2 }

      test "#4" do
        let input = { a0: 0, a1: 1, a2: 2 }
        contract input `shouldEqual` { a0: 0, a2: 2 }

      test "#5 -- Should Not Compile" $ pure unit
--         let input = { a0: 0, a1: 1, a2: 2 }
--         (contract <<< contract) input `shouldEqual` { a0: 0, a2: 2 }

      test "#6 -- Should Not Compile" $ pure unit
--         let input = { a0: 0, a1: 1, a2: 2 }
--         (contract <<< identity <<< contract) input
--           `shouldEqual`
--           { a0: 0, a2: 2 }

      test "#7 -- Should Not Compile" $ pure unit
--         let input = {}
--         contract input `shouldEqual` { a0: 0 }

      test "#8" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3, a4: 4, a5: 5 }
        contract input
          `shouldEqual`
          { a5: 5, a3: 3, a1: 1, a0: 0, a2: 2, a4: 4 }
