module Test.Main
  ( main
  ) where

import Prelude (Unit, discard)

import Effect (Effect)
import Test.Suites.Data.Struct.Contract.Contract (suites) as Contract
import Test.Suites.Data.Struct.RRenameMany (suites) as RRenameMany
import Test.Suites.Data.Struct.RSingleton (suites) as RSingleton
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
   Contract.suites
   RRenameMany.suites
   RSingleton.suites
