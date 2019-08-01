module Test.Suites.Data.Struct.Map.Map
  ( suites
  ) where


import Prelude (discard, (+), (*), (<>), (<<<), ($))

import Data.Struct.Map (map)
import Data.Symbol (SProxy(SProxy))
import Data.Variant (Variant, inj)
import Record.Builder (build)
import Record.Builder (insert, rename) as Builder
import Record (insert, rename) as Record
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

suites :: TestSuite
suites =
  suite "map" do
    suite "Function" do
      test "#0" do
        let input = { a0: 0 }
        let f = map {}
        f input `shouldEqual` { a0: 0 }

      test "#1" do
        let input = { a0: 0 }
        let f = map { a0: \i -> i + 1 }
        f input `shouldEqual` { a0: 1 }

      test "#2" do
        let input = { a0: 0, a1: 1 }
        let f = map { a0: \i -> i + 1, a1: \i -> i * 2 }
        f input `shouldEqual` { a0: 1, a1: 2 }

      test "#3a" do
        let input = { a0: 0, a1: 1, a2: 2 }
        let f =
              Record.insert (SProxy :: SProxy "a3") 3
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a3: 3 }

      test "#3b" do
        let input = { a0: 0, a1: 1, a2: 2 }
        let f =
              map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Record.insert (SProxy :: SProxy "a3") 3
        f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a3: 3 }

      test "#4" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1000, a3: \i -> i + 1000 }
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        f input `shouldEqual` { a0: 1001, a1: 2, a2: 2, a3: 1003 }

      test "#5" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3, a4: 4 }
        let f =
              map { a0: \i -> i + 1000, a3: \i -> i + 1000 }
                <<< map { a0: \i -> i + 2, a4: \i -> i * 4 }
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        shouldEqual
          { a0: 1003, a1: 2, a2: 2, a3: 1003, a4: 16 }
          $ f input

      test "#6a" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              Record.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3 }

      test "#6b" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Record.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
        f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3 }

      test "#7a" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Record.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
                <<< Record.insert (SProxy :: SProxy "a5") 5
        f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3, a5: 5 }

      test "#7b" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Record.insert (SProxy :: SProxy "a5") 5
                <<< Record.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
        f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3, a5: 5 }

      test "#7c" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              Record.insert (SProxy :: SProxy "a5") 5
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Record.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
        f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3, a5: 5 }

      test "#7d" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              Record.insert (SProxy :: SProxy "a5") 5
                <<< Record.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3, a5: 5 }

      test "#8" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1000, a5: \i -> i + 1000 }
                <<< Record.insert (SProxy :: SProxy "a5") 5
                <<< Record.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        f input `shouldEqual` { a0: 1001, a1: 2, a2: 2, a4: 3, a5: 1005 }

      test "#9" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i * 2, a3: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a2: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a1: \i -> i + 1 }

                <<< map { a0: \i -> i * 2, a3: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a2: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a1: \i -> i + 1 }

                <<< map { a0: \i -> i * 2, a3: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a2: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a1: \i -> i + 1 }

                <<< map { a0: \i -> i * 2, a3: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a2: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a1: \i -> i + 1 }
        f input `shouldEqual` { a0: 0, a1: 5, a2: 6, a3: 7 }

    suite "Builder" do
      test "#0" do
        let input = { a0: 0 }
        let f = map {}
        build f input `shouldEqual` { a0: 0 }

      test "#1" do
        let input = { a0: 0 }
        let f = map { a0: \i -> i + 1 }
        build f input `shouldEqual` { a0: 1 }

      test "#2" do
        let input = { a0: 0, a1: 1 }
        let f = map { a0: \i -> i + 1, a1: \i -> i * 2 }
        build f input `shouldEqual` { a0: 1, a1: 2 }

      test "#3a" do
        let input = { a0: 0, a1: 1, a2: 2 }
        let f =
              Builder.insert (SProxy :: SProxy "a3") 3
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        build f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a3: 3 }

      test "#3b" do
        let input = { a0: 0, a1: 1, a2: 2 }
        let f =
              map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Builder.insert (SProxy :: SProxy "a3") 3
        build f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a3: 3 }

      test "#4" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1000, a3: \i -> i + 1000 }
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        build f input `shouldEqual` { a0: 1001, a1: 2, a2: 2, a3: 1003 }

      test "#5" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3, a4: 4 }
        let f =
              map { a0: \i -> i + 1000, a3: \i -> i + 1000 }
                <<< map { a0: \i -> i + 2, a4: \i -> i * 4 }
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        shouldEqual
          { a0: 1003, a1: 2, a2: 2, a3: 1003, a4: 16 }
          $ build f input

      test "#6a" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              Builder.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        build f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3 }

      test "#6b" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Builder.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
        build f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3 }

      test "#7a" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Builder.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
                <<< Builder.insert (SProxy :: SProxy "a5") 5
        build f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3, a5: 5 }

      test "#7b" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Builder.insert (SProxy :: SProxy "a5") 5
                <<< Builder.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
        build f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3, a5: 5 }

      test "#7c" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              Builder.insert (SProxy :: SProxy "a5") 5
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
                <<< Builder.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
        build f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3, a5: 5 }

      test "#7d" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              Builder.insert (SProxy :: SProxy "a5") 5
                <<< Builder.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        build f input `shouldEqual` { a0: 1, a1: 2, a2: 2, a4: 3, a5: 5 }

      test "#8" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i + 1000, a5: \i -> i + 1000 }
                <<< Builder.insert (SProxy :: SProxy "a5") 5
                <<< Builder.rename (SProxy :: SProxy "a3") (SProxy :: SProxy "a4")
                <<< map { a0: \i -> i + 1, a1: \i -> i * 2 }
        build f input `shouldEqual` { a0: 1001, a1: 2, a2: 2, a4: 3, a5: 1005 }

      test "#9" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3 }
        let f =
              map { a0: \i -> i * 2, a3: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a2: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a1: \i -> i + 1 }

                <<< map { a0: \i -> i * 2, a3: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a2: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a1: \i -> i + 1 }

                <<< map { a0: \i -> i * 2, a3: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a2: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a1: \i -> i + 1 }

                <<< map { a0: \i -> i * 2, a3: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a2: \i -> i + 1 }
                <<< map { a0: \i -> i * 2, a1: \i -> i + 1 }
        build f input `shouldEqual` { a0: 0, a1: 5, a2: 6, a3: 7 }

    suite "Variant" do
      test "#0" do
        let l = RLProxy :: RLProxy (Cons "a" Int Nil)
        let s = SProxy :: SProxy "a"
        let value = 10
        let input = inj s value :: Variant (a :: Int)
        let f = map {}
        f input `shouldEqual` input

      test "#1" do
        let l = RLProxy :: RLProxy (Cons "a" Int Nil)
        let s = SProxy :: SProxy "a"
        let value = 10
        let input = inj s value :: Variant (a :: Int)
        let f' i = i + 1
        let f = map { a: f' }
        let result = inj s (f' value) :: Variant (a :: Int)
        f input `shouldEqual` result

      test "#2" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let s0 = SProxy :: SProxy "a0"
        let value = 10
        let input = inj s0 value :: Variant (a0 :: Int, a1 :: Int)
        let f' i = i + 1
        let f = map { a0: f' }
        let result = inj s0 (f' value) :: Variant (a0 :: Int, a1 :: Int)
        f input `shouldEqual` result

      test "#3" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let s0 = SProxy :: SProxy "a0"
        let value = 10
        let input = inj s0 value :: Variant (a0 :: Int, a1 :: Int)
        let f' i = i + 1
        let f = map { a1: f' }
        f input `shouldEqual` input

      test "#4" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let s0 = SProxy :: SProxy "a0"
        let value = 10
        let input = inj s0 value :: Variant (a0 :: Int, a1 :: Int)
        let f0 i = i + 10
        let f1 i = i + 11
        let f = map { a0: f0, a1: f1 }
        let result = inj s0 (f0 value) :: Variant (a0 :: Int, a1 :: Int)
        f input `shouldEqual` result

      test "#5" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let s0 = SProxy :: SProxy "a0"
        let value = 10
        let input = inj s0 value :: Variant (a0 :: Int, a1 :: Int)
        let f0 i = i + 10
        let f1 i = i + 11
        let g0 i = i * 10
        let g1 i = i * 11
        let f = map { a0: f0, a1: f1 } <<< map { a0: g0, a1: g1 }
        let result = inj s0 (f0 (g0 value)) :: Variant (a0 :: Int, a1 :: Int)
        f input `shouldEqual` result

      test "#6" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        let s0 = SProxy :: SProxy "a0"
        let value = 10
        let input = inj s0 value :: Variant (a0 :: Int, a1 :: String)
        let f0 i = i + 10
        let f1 s = s <> s
        let g0 i = i * 10
        let g1 s = s <> s <> s
        let f = map { a0: f0, a1: f1 } <<< map { a0: g0, a1: g1 }
        let result = inj s0 (f0 (g0 value)) :: Variant (a0 :: Int, a1 :: String)
        f input `shouldEqual` result
