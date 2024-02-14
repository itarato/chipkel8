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
    [ testVMInitState,
      testVMPCInc
    ]

testVMInitState :: TestTree
testVMInitState = testCase "VM init state" (assertPc *> assertStack *> assertRegs)
  where
    vm = initVM []
    assertPc = assertEqual "PC is 0x200" 0x200 $ pc vm
    assertStack = assertEqual "Stack size" 16 $ length $ stack vm
    assertRegs = assertEqual "Regs size" 16 $ length $ regs vm

testVMPCInc :: TestTree
testVMPCInc = testCase "PC inc" (assertDefault *> assertIncreased)
  where
    vm = initVM []
    assertDefault = assertEqual "PC is 200" 0x200 $ pc vm
    vm' = withPCInc vm
    assertIncreased = assertEqual "PC is 202" 0x202 $ pc vm'