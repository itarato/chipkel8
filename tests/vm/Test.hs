{-# LANGUAGE BinaryLiterals #-}

module Main where

import Config (displayWidth)
import Data.List
import Data.Ord
import qualified Data.Vector as V
import Debug.Trace
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
      testVMWithCall,
      testVMWithDrawSprite,
      testVMWithDrawSpriteWithOverrideNo,
      testVMWithDrawSpriteWithOverrideYes
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

testVMWithDrawSprite :: TestTree
testVMWithDrawSprite = testCase "Display draw sprite" (assertVF *> assertTopLeft *> assertBottomLeft *> assertTopRight *> assertBottomRight)
  where
    vm = initVM []
    -- 6 pixel on the right, 2 on the left
    _memory = memory vm
    memory' = (V.//) _memory [(0x150, 0b10010110), (0x151, 0b11111001)]
    vm' = vm {iReg = 0x150, regs = V.fromList [0, 0, 58, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], memory = memory'}
    vm'' = withDrawSprite 2 3 2 vm'
    _displayBuffer = displayBuffer vm''
    topLeftPixels = (V.!) _displayBuffer <$> [displayWidth * 3, displayWidth * 3 + 1]
    bottomLeftPixels = (V.!) _displayBuffer <$> [displayWidth * 4, displayWidth * 4 + 1]
    topRightPixels = (V.!) _displayBuffer . (+ (displayWidth * 3)) <$> [58 .. 63]
    bottomRightPixels = (V.!) _displayBuffer . (+ (displayWidth * 4)) <$> [58 .. 63]
    assertVF = assertEqual "No pixel deletion" 0 $ (V.!) (regs vm'') 0xf
    assertTopLeft = assertEqual "Top left segment" [1, 0] topLeftPixels
    assertBottomLeft = assertEqual "Bottom left segment" [0, 1] bottomLeftPixels
    assertTopRight = assertEqual "Top right segment" [1, 0, 0, 1, 0, 1] topRightPixels
    assertBottomRight = assertEqual "Bottom right segment" [1, 1, 1, 1, 1, 0] bottomRightPixels

testVMWithDrawSpriteWithOverrideNo :: TestTree
testVMWithDrawSpriteWithOverrideNo = testCase "Display draw sprite with non removed pixel" (assertVF *> assertTopLeft *> assertBottomLeft *> assertTopRight *> assertBottomRight)
  where
    vm = initVM []
    -- 6 pixel on the right, 2 on the left
    _memory = memory vm
    memory' = (V.//) _memory [(0x150, 0b10010110), (0x151, 0b11111001)]
    _displayBuffer' = displayBuffer vm
    _displayBuffer'' = (V.//) _displayBuffer' [(displayWidth * 3 + 1, 1)] -- This won't be erased.
    vm' = vm {iReg = 0x150, regs = V.fromList [0, 0, 58, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], memory = memory', displayBuffer = _displayBuffer''}
    vm'' = withDrawSprite 2 3 2 vm'
    _displayBuffer = displayBuffer vm''
    topLeftPixels = (V.!) _displayBuffer <$> [displayWidth * 3, displayWidth * 3 + 1]
    bottomLeftPixels = (V.!) _displayBuffer <$> [displayWidth * 4, displayWidth * 4 + 1]
    topRightPixels = (V.!) _displayBuffer . (+ (displayWidth * 3)) <$> [58 .. 63]
    bottomRightPixels = (V.!) _displayBuffer . (+ (displayWidth * 4)) <$> [58 .. 63]
    assertVF = assertEqual "No pixel deletion" 0 $ (V.!) (regs vm'') 0xf
    assertTopLeft = assertEqual "Top left segment" [1, 1] topLeftPixels -- Second pixel remains 1
    assertBottomLeft = assertEqual "Bottom left segment" [0, 1] bottomLeftPixels
    assertTopRight = assertEqual "Top right segment" [1, 0, 0, 1, 0, 1] topRightPixels
    assertBottomRight = assertEqual "Bottom right segment" [1, 1, 1, 1, 1, 0] bottomRightPixels

testVMWithDrawSpriteWithOverrideYes :: TestTree
testVMWithDrawSpriteWithOverrideYes = testCase "Display draw sprite with removed pixel" (assertVF *> assertTopLeft *> assertBottomLeft *> assertTopRight *> assertBottomRight)
  where
    vm = initVM []
    -- 6 pixel on the right, 2 on the left
    _memory = memory vm
    memory' = (V.//) _memory [(0x150, 0b10010110), (0x151, 0b11111001)]
    _displayBuffer' = displayBuffer vm
    _displayBuffer'' = (V.//) _displayBuffer' [(displayWidth * 3, 1)] -- This will be erased.
    vm' = vm {iReg = 0x150, regs = V.fromList [0, 0, 58, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], memory = memory', displayBuffer = _displayBuffer''}
    vm'' = withDrawSprite 2 3 2 vm'
    _displayBuffer = displayBuffer vm''
    topLeftPixels = (V.!) _displayBuffer <$> [displayWidth * 3, displayWidth * 3 + 1]
    bottomLeftPixels = (V.!) _displayBuffer <$> [displayWidth * 4, displayWidth * 4 + 1]
    topRightPixels = (V.!) _displayBuffer . (+ (displayWidth * 3)) <$> [58 .. 63]
    bottomRightPixels = (V.!) _displayBuffer . (+ (displayWidth * 4)) <$> [58 .. 63]
    assertVF = assertEqual "Yes pixel deletion" 1 $ (V.!) (regs vm'') 0xf
    assertTopLeft = assertEqual "Top left segment" [0, 0] topLeftPixels -- First pixel disappeared
    assertBottomLeft = assertEqual "Bottom left segment" [0, 1] bottomLeftPixels
    assertTopRight = assertEqual "Top right segment" [1, 0, 0, 1, 0, 1] topRightPixels
    assertBottomRight = assertEqual "Bottom right segment" [1, 1, 1, 1, 1, 0] bottomRightPixels
