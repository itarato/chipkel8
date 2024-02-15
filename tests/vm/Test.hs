module Main where

import Data.List
import Data.Ord
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit
import VM

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
  testGroup
    "VM tests"
    [ testVMInitState,
      testVMWithPCInc,
      testVMWithReturnFromStack,
      testVMWithCall
    ]

testVMInitState :: TestTree
testVMInitState = testCase "VM init state" (assertPc *> assertStack *> assertRegs)
  where
    vm = initVM []
    assertPc = assertEqual "PC is 0x200" 0x200 $ pc vm
    assertStack = assertEqual "Stack size" 16 $ length $ stack vm
    assertRegs = assertEqual "Regs size" 16 $ length $ regs vm

testVMWithPCInc :: TestTree
testVMWithPCInc = testCase "PC inc" (assertDefault *> assertIncreased)
  where
    vm = initVM []
    assertDefault = assertEqual "PC is 200" 0x200 $ pc vm
    vm' = withPCInc vm
    assertIncreased = assertEqual "PC is 202" 0x202 $ pc vm'

testVMWithReturnFromStack :: TestTree
testVMWithReturnFromStack = testCase "Return from stack" (assertPC *> assertSP)
  where
    vm = initVM []
    _stack = stack vm
    vm' = vm {stack = (V.//) _stack [(0xF, 123)], sp = 0xE}
    vm'' = withReturnFromStack vm'
    assertPC = assertEqual "PC is 123" 123 $ pc vm''
    assertSP = assertEqual "SP is 0XF" 0xF $ sp vm''

testVMWithCall :: TestTree
testVMWithCall = testCase "Call" (assertPC *> assertSP *> assertStack)
  where
    vm = withCall 123 $ initVM []
    _stack = stack vm
    assertPC = assertEqual "PC is 123" 123 $ pc vm
    assertSP = assertEqual "SP is 0xE" 0xE $ sp vm
    assertStack = assertEqual "Last addr is on stack" 0x200 $ (V.!) _stack 0xF
