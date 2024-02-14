module Main where

import Data.List
import Data.Ord
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import VM

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
  testGroup
    "VM tests"
    [ testVMInitState
    ]

testVMInitState :: TestTree
testVMInitState = testCase "PC is 0x200" (assertPc *> assertStack *> assertRegs)
  where
    vm = initVM []
    assertPc = assertEqual "PC is 0x200" 0x200 $ pc vm
    assertStack = assertEqual "Stack size" 16 $ length $ stack vm
    assertRegs = assertEqual "Regs size" 16 $ length $ regs vm
