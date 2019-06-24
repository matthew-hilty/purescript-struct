module Test.Suites.Data.Struct.RRenameMany
  ( suites
  ) where

import Prelude (discard, map, (<<<), ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(Left, Right))
import Data.List ((:))
import Data.List (List(Nil)) as List
import Data.Maybe (Maybe(Just, Nothing))
import Data.Struct.RenameMany (rrenameMany)
import Data.Symbol (SProxy(SProxy))
import Record.Builder (build)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

data Arbitrary a (s :: Symbol) = Arbitrary

foreign import foreignValue :: Json

suites :: TestSuite
suites =
  suite "rrenameMany" do
    suite "Builder" do

      test "#0" do
        let value = {}
        let builder = rrenameMany {}
        build builder value `shouldEqual` value

      test "#1" do
        let value = { a0: 0 }
        let builder = rrenameMany { a0: SProxy :: SProxy "b0" }
        build builder value `shouldEqual` { b0: value.a0 }

      test "#2" do
        let value = { a0: 0 }
        let builder = rrenameMany {}
        build builder value `shouldEqual` value

      test "#3" do
        let value = { a0: 0, a1: "1", a2: true }
        let builder = rrenameMany {}
        build builder value `shouldEqual` value

      test "#4" do
        let value = { a0: 0, a1: "1", a2: true }
        let builder =
              rrenameMany
                { a0: SProxy :: SProxy "b0"
                , a1: SProxy :: SProxy "b1"
                , a2: SProxy :: SProxy "b2"
                }
        build builder value
          `shouldEqual`
          { b0: value.a0, b1: value.a1, b2: value.a2 }

      test "#5" do
        let value = { a0: 0, a1: "1", a2: true }
        let builder =
              rrenameMany
                { a0: Arbitrary :: Arbitrary Int "b0"
                , a1: Arbitrary :: Arbitrary String "b1"
                , a2: Arbitrary :: Arbitrary {} "b2"
                }
        build builder value
          `shouldEqual`
          { b0: value.a0, b1: value.a1, b2: value.a2 }

      test "#6" do
        let value =
              { a0: 0
              , a1: "1"
              , a2: true
              , a3: Just 3
              , a4: Left "4" :: Either String Int
              , a5: [5]
              , a6: 6 : List.Nil
              , a7: { c0: 70, c1: "71" }
              }
        let builder = rrenameMany {}
        build builder value `shouldEqual` value

      test "#7" do
        let value =
              { a0: 0
              , a1: "1"
              , a2: true
              , a3: Just 3
              , a4: (Left "4" :: Either String Int)
              , a5: [5]
              , a6: 6 : List.Nil
              , a7: { c0: 70, c1: "71" }
              }
        let builder = rrenameMany { a3: SProxy :: SProxy "b3" }
        build builder value
          `shouldEqual`
          { a0: value.a0
          , a1: value.a1
          , a2: value.a2
          , b3: value.a3
          , a4: value.a4
          , a5: value.a5
          , a6: value.a6
          , a7: value.a7
          }

      test "#8" do
        let value =
              { a0: 0
              , a1: "1"
              , a2: true
              , a3: Just 3
              , a4: (Left "4" :: Either String Int)
              , a5: [5]
              , a6: 6 : List.Nil
              , a7: { c0: 70, c1: "71" }
              }
        let builder =
              rrenameMany
                { a0: SProxy :: SProxy "b0"
                , a1: SProxy :: SProxy "b1"
                , a2: SProxy :: SProxy "b2"
                , a3: SProxy :: SProxy "b3"
                , a4: SProxy :: SProxy "b4"
                , a5: SProxy :: SProxy "b5"
                , a6: SProxy :: SProxy "b6"
                , a7: SProxy :: SProxy "b7"
                }
        build builder value
          `shouldEqual`
          { b0: value.a0
          , b1: value.a1
          , b2: value.a2
          , b3: value.a3
          , b4: value.a4
          , b5: value.a5
          , b6: value.a6
          , b7: value.a7
          }

      test "#9" do
        let value =
              { a0: 100
              , b0: 200
              , a1: "101"
              , b1: "201"
              , a2: true
              , b2: false
              , a3: Just 3
              , b3: Nothing :: Maybe Int
              , a4: Left "4" :: Either String Int
              , b4: Right 4 :: Either String Int
              , a5: [105]
              , b5: [205]
              , a6: 106 : List.Nil
              , b6: 206 : List.Nil
              , a7: { c0: 1070, c1: "1071" }
              , b7: { c0: 2070, c1: "2071" }
              }
        let builder = rrenameMany {}
        build builder value `shouldEqual` value

      test "#10" do
        let value =
              { a0: 100
              , b0: 200
              , a1: "101"
              , b1: "201"
              , a2: true
              , b2: false
              , a3: Just 3
              , b3: (Nothing :: Maybe Int)
              , a4: (Left "4" :: Either String Int)
              , b4: (Right 4 :: Either String Int)
              , a5: [105]
              , b5: [205]
              , a6: 106 : List.Nil
              , b6: 206 : List.Nil
              , a7: { c0: 1070, c1: "1071" }
              , b7: { c0: 2070, c1: "2071" }
              }
        let builder = rrenameMany { a3: SProxy :: SProxy "c3" }
        build builder value
          `shouldEqual`
          { a0: value.a0
          , b0: value.b0
          , a1: value.a1
          , b1: value.b1
          , a2: value.a2
          , b2: value.b2
          , c3: value.a3
          , b3: value.b3
          , a4: value.a4
          , b4: value.b4
          , a5: value.a5
          , b5: value.b5
          , a6: value.a6
          , b6: value.b6
          , a7: value.a7
          , b7: value.b7
          }

      test "#11" do
        let value =
              { a0: 100
              , b0: 200
              , a1: "101"
              , b1: "201"
              , a2: true
              , b2: false
              , a3: Just 3
              , b3: Nothing :: Maybe Int
              , a4: Left "4" :: Either String Int
              , b4: Right 4 :: Either String Int
              , a5: [105]
              , b5: [205]
              , a6: 106 : List.Nil
              , b6: 206 : List.Nil
              , a7: { c0: 1070, c1: "1071" }
              , b7: { c0: 2070, c1: "2071" }
              }
        let builder =
              rrenameMany
                { a0: SProxy :: SProxy "c0"
                , a1: SProxy :: SProxy "c1"
                , a2: SProxy :: SProxy "c2"
                , a3: SProxy :: SProxy "c3"
                , a4: SProxy :: SProxy "c4"
                , a5: SProxy :: SProxy "c5"
                , a6: SProxy :: SProxy "c6"
                , a7: SProxy :: SProxy "c7"
                }
        build builder value
          `shouldEqual`
          { c0: value.a0
          , b0: value.b0
          , c1: value.a1
          , b1: value.b1
          , c2: value.a2
          , b2: value.b2
          , c3: value.a3
          , b3: value.b3
          , c4: value.a4
          , b4: value.b4
          , c5: value.a5
          , b5: value.b5
          , c6: value.a6
          , b6: value.b6
          , c7: value.a7
          , b7: value.b7
          }

      test "#12" do
        let value =
              { a0: 100
              , b0: 200
              , a1: "101"
              , b1: "201"
              , a2: true
              , b2: false
              , a3: Just 3
              , b3: Nothing :: Maybe Int
              , a4: Left "4" :: Either String Int
              , b4: Right 4 :: Either String Int
              , a5: [105]
              , b5: [205]
              , a6: 106 : List.Nil
              , b6: 206 : List.Nil
              , a7: { c0: 1070, c1: "1071" }
              , b7: { c0: 2070, c1: "2071" }
              }
        let builder =
              rrenameMany
                { a7: SProxy :: SProxy "c7"
                , a5: SProxy :: SProxy "c5"
                , a3: SProxy :: SProxy "c3"
                , a1: SProxy :: SProxy "c1"
                , a0: SProxy :: SProxy "c0"
                , a2: SProxy :: SProxy "c2"
                , a4: SProxy :: SProxy "c4"
                , a6: SProxy :: SProxy "c6"
                }
        build builder value
          `shouldEqual`
          { c0: value.a0
          , b0: value.b0
          , c1: value.a1
          , b1: value.b1
          , c2: value.a2
          , b2: value.b2
          , c3: value.a3
          , b3: value.b3
          , c4: value.a4
          , b4: value.b4
          , c5: value.a5
          , b5: value.b5
          , c6: value.a6
          , b6: value.b6
          , c7: value.a7
          , b7: value.b7
          }

      test "#13" do
        let value =
              { a0: 100
              , b0: 200
              , a1: "101"
              , b1: "201"
              , a2: true
              , b2: false
              , a3: Just 3
              , b3: Nothing :: Maybe Int
              , a4: Left "4" :: Either String Int
              , b4: Right 4 :: Either String Int
              , a5: [105]
              , b5: [205]
              , a6: 106 : List.Nil
              , b6: 206 : List.Nil
              , a7: { c0: 1070, c1: "1071" }
              , b7: { c0: 2070, c1: "2071" }
              }
        let builder =
              rrenameMany
                { a0: SProxy :: SProxy "c0"
                , b0: SProxy :: SProxy "d0"
                , a1: SProxy :: SProxy "c1"
                , b1: SProxy :: SProxy "d1"
                , a2: SProxy :: SProxy "c2"
                , b2: SProxy :: SProxy "d2"
                , a3: SProxy :: SProxy "c3"
                , b3: SProxy :: SProxy "d3"
                , a4: SProxy :: SProxy "c4"
                , b4: SProxy :: SProxy "d4"
                , a5: SProxy :: SProxy "c5"
                , b5: SProxy :: SProxy "d5"
                , a6: SProxy :: SProxy "c6"
                , b6: SProxy :: SProxy "d6"
                , a7: SProxy :: SProxy "c7"
                , b7: SProxy :: SProxy "d7"
                }
        build builder value
          `shouldEqual`
          { c0: value.a0
          , d0: value.b0
          , c1: value.a1
          , d1: value.b1
          , c2: value.a2
          , d2: value.b2
          , c3: value.a3
          , d3: value.b3
          , c4: value.a4
          , d4: value.b4
          , c5: value.a5
          , d5: value.b5
          , c6: value.a6
          , d6: value.b6
          , c7: value.a7
          , d7: value.b7
          }

      suite "Inference" do
        test "#0" do
          let value = decodeJson foreignValue
          let builder = rrenameMany { a0: SProxy :: SProxy "b0" }
          map (build builder) value `shouldEqual` Right { b0: 0 }

        test "#1" do
          let value = decodeJson foreignValue
          let builder =
                rrenameMany { i0: SProxy :: SProxy "j0" }
                  <<< rrenameMany { h0: SProxy :: SProxy "i0" }
                  <<< rrenameMany { g0: SProxy :: SProxy "h0" }
                  <<< rrenameMany
                            { f0: SProxy :: SProxy "g0"
                            , a2: SProxy :: SProxy "b2"
                            }
                  <<< rrenameMany { c1: SProxy :: SProxy "d1" }
                  <<< rrenameMany { e0: SProxy :: SProxy "f0" }
                  <<< rrenameMany { b1: SProxy :: SProxy "c1" }
                  <<< rrenameMany { d0: SProxy :: SProxy "e0" }
                  <<< rrenameMany
                            { c0: SProxy :: SProxy "d0"
                            , a1: SProxy :: SProxy "b1"
                            }
                  <<< rrenameMany { b0: SProxy :: SProxy "c0" }
                  <<< rrenameMany { a0: SProxy :: SProxy "b0" }
          map (build builder) value
            `shouldEqual`
            Right { j0: 0, d1: 1, b2: 2, a3: 3 }

    suite "Function" do

        test "#0" do
          let value = {}
          let result = rrenameMany {} value
          result `shouldEqual` value

        test "#1" do
          let value = { a0: 0 }
          let result = rrenameMany { a0: SProxy :: SProxy "b0" } value
          result `shouldEqual` { b0: 0 }

        test "#2" do
          let value = { a0: 0 }
          let result = rrenameMany {} value
          result `shouldEqual` value

        test "#3" do
          let value = { a0: 0, a1: "1", a2: true }
          let result = rrenameMany {} value
          result `shouldEqual` value

        test "#4" do
          let value = { a0: 0, a1: "1", a2: true }
          let result =
                rrenameMany
                  { a0: SProxy :: SProxy "b0"
                  , a1: SProxy :: SProxy "b1"
                  , a2: SProxy :: SProxy "b2"
                  }
                  value
          result `shouldEqual` { b0: value.a0, b1: value.a1, b2: value.a2 }

        test "#5" do
          let value = { a0: 0, a1: "1", a2: true }
          let result =
                rrenameMany
                  { a0: Arbitrary :: Arbitrary Int "b0"
                  , a1: Arbitrary :: Arbitrary String "b1"
                  , a2: Arbitrary :: Arbitrary {} "b2"
                  }
                  value
          result `shouldEqual` { b0: value.a0, b1: value.a1, b2: value.a2 }

        test "#6" do
          let value =
                { a0: 0
                , a1: "1"
                , a2: true
                , a3: Just 3
                , a4: Left "4" :: Either String Int
                , a5: [5]
                , a6: 6 : List.Nil
                , a7: { c0: 70, c1: "71" }
                }
          let result = rrenameMany {} value
          result `shouldEqual` value

        test "#7" do
          let value =
                { a0: 0
                , a1: "1"
                , a2: true
                , a3: Just 3
                , a4: Left "4" :: Either String Int
                , a5: [5]
                , a6: 6 : List.Nil
                , a7: { c0: 70, c1: "71" }
                }
          let result = rrenameMany { a3: SProxy :: SProxy "b3" } value
          result
            `shouldEqual`
            { a0: value.a0
            , a1: value.a1
            , a2: value.a2
            , b3: value.a3
            , a4: value.a4
            , a5: value.a5
            , a6: value.a6
            , a7: value.a7
            }

        test "#8" do
          let value =
                { a0: 0
                , a1: "1"
                , a2: true
                , a3: Just 3
                , a4: Left "4" :: Either String Int
                , a5: [5]
                , a6: 6 : List.Nil
                , a7: { c0: 70, c1: "71" }
                }
          let result =
                rrenameMany
                  { a0: SProxy :: SProxy "b0"
                  , a1: SProxy :: SProxy "b1"
                  , a2: SProxy :: SProxy "b2"
                  , a3: SProxy :: SProxy "b3"
                  , a4: SProxy :: SProxy "b4"
                  , a5: SProxy :: SProxy "b5"
                  , a6: SProxy :: SProxy "b6"
                  , a7: SProxy :: SProxy "b7"
                  }
                  value
          result
            `shouldEqual`
            { b0: value.a0
            , b1: value.a1
            , b2: value.a2
            , b3: value.a3
            , b4: value.a4
            , b5: value.a5
            , b6: value.a6
            , b7: value.a7
            }

        test "#9" do
          let value =
                { a0: 100
                , b0: 200
                , a1: "101"
                , b1: "201"
                , a2: true
                , b2: false
                , a3: Just 3
                , b3: Nothing :: Maybe Int
                , a4: Left "4" :: Either String Int
                , b4: Right 4 :: Either String Int
                , a5: [105]
                , b5: [205]
                , a6: 106 : List.Nil
                , b6: 206 : List.Nil
                , a7: { c0: 1070, c1: "1071" }
                , b7: { c0: 2070, c1: "2071" }
                }
          let result = rrenameMany {} value
          result `shouldEqual` value

        test "#10" do
          let value =
                { a0: 100
                , b0: 200
                , a1: "101"
                , b1: "201"
                , a2: true
                , b2: false
                , a3: Just 3
                , b3: Nothing :: Maybe Int
                , a4: Left "4" :: Either String Int
                , b4: Right 4 :: Either String Int
                , a5: [105]
                , b5: [205]
                , a6: 106 : List.Nil
                , b6: 206 : List.Nil
                , a7: { c0: 1070, c1: "1071" }
                , b7: { c0: 2070, c1: "2071" }
                }
          let result = rrenameMany { a3: SProxy :: SProxy "c3" } value
          result
            `shouldEqual`
            { a0: value.a0
            , b0: value.b0
            , a1: value.a1
            , b1: value.b1
            , a2: value.a2
            , b2: value.b2
            , c3: value.a3
            , b3: value.b3
            , a4: value.a4
            , b4: value.b4
            , a5: value.a5
            , b5: value.b5
            , a6: value.a6
            , b6: value.b6
            , a7: value.a7
            , b7: value.b7
            }

        test "#11" do
          let value =
                { a0: 100
                , b0: 200
                , a1: "101"
                , b1: "201"
                , a2: true
                , b2: false
                , a3: Just 3
                , b3: Nothing :: Maybe Int
                , a4: Left "4" :: Either String Int
                , b4: Right 4 :: Either String Int
                , a5: [105]
                , b5: [205]
                , a6: 106 : List.Nil
                , b6: 206 : List.Nil
                , a7: { c0: 1070, c1: "1071" }
                , b7: { c0: 2070, c1: "2071" }
                }
          let result =
                rrenameMany
                  { a0: SProxy :: SProxy "c0"
                  , a1: SProxy :: SProxy "c1"
                  , a2: SProxy :: SProxy "c2"
                  , a3: SProxy :: SProxy "c3"
                  , a4: SProxy :: SProxy "c4"
                  , a5: SProxy :: SProxy "c5"
                  , a6: SProxy :: SProxy "c6"
                  , a7: SProxy :: SProxy "c7"
                  }
                  value
          result
            `shouldEqual`
            { c0: value.a0
            , b0: value.b0
            , c1: value.a1
            , b1: value.b1
            , c2: value.a2
            , b2: value.b2
            , c3: value.a3
            , b3: value.b3
            , c4: value.a4
            , b4: value.b4
            , c5: value.a5
            , b5: value.b5
            , c6: value.a6
            , b6: value.b6
            , c7: value.a7
            , b7: value.b7
            }

        test "#12" do
          let value =
                { a0: 100
                , b0: 200
                , a1: "101"
                , b1: "201"
                , a2: true
                , b2: false
                , a3: Just 3
                , b3: Nothing :: Maybe Int
                , a4: Left "4" :: Either String Int
                , b4: Right 4 :: Either String Int
                , a5: [105]
                , b5: [205]
                , a6: 106 : List.Nil
                , b6: 206 : List.Nil
                , a7: { c0: 1070, c1: "1071" }
                , b7: { c0: 2070, c1: "2071" }
                }
          let result =
                rrenameMany
                  { a7: SProxy :: SProxy "c7"
                  , a5: SProxy :: SProxy "c5"
                  , a3: SProxy :: SProxy "c3"
                  , a1: SProxy :: SProxy "c1"
                  , a0: SProxy :: SProxy "c0"
                  , a2: SProxy :: SProxy "c2"
                  , a4: SProxy :: SProxy "c4"
                  , a6: SProxy :: SProxy "c6"
                  }
                  value
          result
            `shouldEqual`
            { c0: value.a0
            , b0: value.b0
            , c1: value.a1
            , b1: value.b1
            , c2: value.a2
            , b2: value.b2
            , c3: value.a3
            , b3: value.b3
            , c4: value.a4
            , b4: value.b4
            , c5: value.a5
            , b5: value.b5
            , c6: value.a6
            , b6: value.b6
            , c7: value.a7
            , b7: value.b7
            }

        test "#13" do
          let value =
                { a0: 100
                , b0: 200
                , a1: "101"
                , b1: "201"
                , a2: true
                , b2: false
                , a3: Just 3
                , b3: Nothing :: Maybe Int
                , a4: Left "4" :: Either String Int
                , b4: Right 4 :: Either String Int
                , a5: [105]
                , b5: [205]
                , a6: 106 : List.Nil
                , b6: 206 : List.Nil
                , a7: { c0: 1070, c1: "1071" }
                , b7: { c0: 2070, c1: "2071" }
                }
          let result =
                rrenameMany
                  { a0: SProxy :: SProxy "c0"
                  , b0: SProxy :: SProxy "d0"
                  , a1: SProxy :: SProxy "c1"
                  , b1: SProxy :: SProxy "d1"
                  , a2: SProxy :: SProxy "c2"
                  , b2: SProxy :: SProxy "d2"
                  , a3: SProxy :: SProxy "c3"
                  , b3: SProxy :: SProxy "d3"
                  , a4: SProxy :: SProxy "c4"
                  , b4: SProxy :: SProxy "d4"
                  , a5: SProxy :: SProxy "c5"
                  , b5: SProxy :: SProxy "d5"
                  , a6: SProxy :: SProxy "c6"
                  , b6: SProxy :: SProxy "d6"
                  , a7: SProxy :: SProxy "c7"
                  , b7: SProxy :: SProxy "d7"
                  }
                  value
          result
            `shouldEqual`
            { c0: value.a0
            , d0: value.b0
            , c1: value.a1
            , d1: value.b1
            , c2: value.a2
            , d2: value.b2
            , c3: value.a3
            , d3: value.b3
            , c4: value.a4
            , d4: value.b4
            , c5: value.a5
            , d5: value.b5
            , c6: value.a6
            , d6: value.b6
            , c7: value.a7
            , d7: value.b7
            }

        suite "Inference" do
          test "#0" do
            let value = decodeJson foreignValue
            let result = map (rrenameMany { a0: SProxy :: SProxy "b0" }) value
            result `shouldEqual` Right { b0: 0 }

          test "#1" do
            let value = decodeJson foreignValue
            let result =
                  map (rrenameMany { i0: SProxy :: SProxy "j0" })
                    <<< map (rrenameMany { h0: SProxy :: SProxy "i0" })
                    <<< map (rrenameMany { g0: SProxy :: SProxy "h0" })
                    <<< map (rrenameMany
                              { f0: SProxy :: SProxy "g0"
                              , a2: SProxy :: SProxy "b2"
                              })
                    <<< map (rrenameMany { c1: SProxy :: SProxy "d1" })
                    <<< map (rrenameMany { e0: SProxy :: SProxy "f0" })
                    <<< map (rrenameMany { b1: SProxy :: SProxy "c1" })
                    <<< map (rrenameMany { d0: SProxy :: SProxy "e0" })
                    <<< map (rrenameMany
                              { c0: SProxy :: SProxy "d0"
                              , a1: SProxy :: SProxy "b1"
                              })
                    <<< map (rrenameMany { b0: SProxy :: SProxy "c0" })
                    <<< map (rrenameMany { a0: SProxy :: SProxy "b0" })
                    $ value
            result `shouldEqual` Right { j0: 0, d1: 1, b2: 2, a3: 3 }
