module VM where

import Config
import Control.Exception
import Data.Bits
import Data.List.Extra
import Data.Vector
import qualified Data.Vector as V
import Data.Word
import Debug.Trace (trace, traceId, traceShowId)
import Shared
import Text.Printf

data VM = MakeVM
  { memory :: V.Vector Word8,
    pc :: Word16,
    -- Stack pointer is always pointing to the top-1 (first) empty spot.
    -- Push must be stack[sp]=v;sp--.
    -- Pop must be sp++;v=stack[sp].
    -- Stack cleanup not necessary.
    sp :: Word8,
    stack :: V.Vector Word16,
    regs :: V.Vector Word8,
    iReg :: Word16,
    soundReg :: Word8,
    timerReg :: Word8,
    displayBuffer :: V.Vector Word8
  }
  deriving (Show)

debugVM :: VM -> IO ()
debugVM vm = do
  printf "PC: %04X I: %04X\n" (pc vm) (iReg vm)
  printf "Tim: %02X Snd: %02X\n" (timerReg vm) (soundReg vm)
  printf "Regs: %s\n" (show $ regs vm)
  printf "Stack: %s SP: %d\n" (show $ stack vm) (sp vm)

traceWithName :: (Show a) => String -> a -> a
traceWithName msg v = trace (msg Prelude.++ ": " Prelude.++ show v) v

traceOpcode :: Word16 -> (Word8, Word8) -> (Word8, Word8)
traceOpcode _pc (a, b) = trace (printf "\x1B[93mPC\x1B[0m %04X \x1B[90m=>\x1B[0m \x1B[93mOP\x1B[0m %02X%02X" _pc a b) (a, b)

initMemory :: [Word8] -> V.Vector Word8
initMemory program = _memory
  where
    fonts = V.fromList [0xF0, 0x90, 0x90, 0x90, 0xF0, 0x20, 0x60, 0x20, 0x20, 0x70, 0xF0, 0x10, 0xF0, 0x80, 0xF0, 0xF0, 0x10, 0xF0, 0x10, 0xF0, 0x90, 0x90, 0xF0, 0x10, 0x10, 0xF0, 0x80, 0xF0, 0x10, 0xF0, 0xF0, 0x80, 0xF0, 0x90, 0xF0, 0xF0, 0x10, 0x20, 0x40, 0x40, 0xF0, 0x90, 0xF0, 0x90, 0xF0, 0xF0, 0x90, 0xF0, 0x10, 0xF0, 0xF0, 0x90, 0xF0, 0x90, 0x90, 0xE0, 0x90, 0xE0, 0x90, 0xE0, 0xF0, 0x80, 0x80, 0x80, 0xF0, 0xE0, 0x90, 0x90, 0x90, 0xE0, 0xF0, 0x80, 0xF0, 0x80, 0xF0, 0xF0, 0x80, 0xF0, 0x80, 0x80]
    fontsSize = V.length fonts
    prefixSize = 0x200
    suffixSize = 0xE00
    programSize = Prelude.length program
    prefixUninitialized = V.replicate (prefixSize - fontsSize) 0
    suffixUninitialized = V.replicate (suffixSize - programSize) 0
    _program = V.fromList program
    prefix = (V.++) fonts prefixUninitialized
    suffix = (V.++) _program suffixUninitialized
    _memory = (V.++) prefix suffix

initDisplayBuffer :: V.Vector Word8
initDisplayBuffer = V.replicate displayBufferSize 0

initVM :: [Word8] -> VM
initVM _program =
  MakeVM
    { memory = initMemory _program,
      --  Most Chip-8 programs start at location 0x200 (512), but some begin at 0x600 (1536). Programs beginning at 0x600 are intended for the ETI 660 computer.
      pc = 0x200,
      sp = 0xF,
      stack = V.replicate 0x10 0,
      regs = V.replicate 0x10 0,
      iReg = 0,
      soundReg = 0,
      timerReg = 0,
      displayBuffer = initDisplayBuffer
    }

opCode :: VM -> (Word8, Word8)
opCode vm = (opCodeHi, opCodeLo)
  where
    _memory = memory vm
    _pc = fromIntegral $ pc vm
    opCodeHi = (V.!) _memory _pc
    opCodeLo = (V.!) _memory (_pc + 1)

withPC :: Word16 -> VM -> VM
withPC newPC vm = vm {pc = newPC}

withPCInc :: VM -> VM
withPCInc vm = let _pc = pc vm in vm {pc = _pc + 2}

withClearedDisplay :: VM -> VM
withClearedDisplay vm = vm {displayBuffer = initDisplayBuffer}

withReturnFromStack :: VM -> VM
withReturnFromStack vm = vm {pc = newPC, sp = newSP}
  where
    _pc = pc vm
    _sp = sp vm
    _stack = stack vm
    newPC = (V.!) _stack (fromIntegral _sp + 1)
    newSP = _sp + 1

withCall :: Word16 -> VM -> VM
withCall addr vm = vm {pc = addr, sp = newSP, stack = newStack}
  where
    _pc = pc vm
    _sp = sp vm
    _stack = stack vm
    newStack = (V.//) _stack [(fromIntegral _sp, _pc)]
    newSP = _sp - 1

withSkipInstructionIfRegEqual :: Int -> Word8 -> VM -> VM
withSkipInstructionIfRegEqual regIdx cmp vm = vm {pc = newPC}
  where
    _pc = pc vm
    _regs = regs vm
    regVal = (V.!) _regs regIdx
    newPC = if regVal == cmp then _pc + 4 else _pc + 2

withSkipInstructionIfRegNotEqual :: Int -> Word8 -> VM -> VM
withSkipInstructionIfRegNotEqual regIdx cmp vm = vm {pc = newPC}
  where
    _pc = pc vm
    _regs = regs vm
    regVal = (V.!) _regs regIdx
    newPC = if regVal == cmp then _pc + 2 else _pc + 4

withSkipInstructionIfTwoRegsEqual :: Int -> Int -> VM -> VM
withSkipInstructionIfTwoRegsEqual lhsRegIdx rhsRegIdx vm = vm {pc = newPC}
  where
    _pc = pc vm
    _regs = regs vm
    lhsRegVal = (V.!) _regs lhsRegIdx
    rhsRegVal = (V.!) _regs rhsRegIdx
    newPC = if lhsRegVal == rhsRegVal then _pc + 4 else _pc + 2

withSkipInstructionIfTwoRegsNotEqual :: Int -> Int -> VM -> VM
withSkipInstructionIfTwoRegsNotEqual lhsRegIdx rhsRegIdx vm = vm {pc = newPC}
  where
    _pc = pc vm
    _regs = regs vm
    lhsRegVal = (V.!) _regs lhsRegIdx
    rhsRegVal = (V.!) _regs rhsRegIdx
    newPC = if lhsRegVal == rhsRegVal then _pc + 2 else _pc + 4

withLoadValueToReg :: Int -> Word8 -> VM -> VM
withLoadValueToReg regIdx value vm = vm {regs = newRegs}
  where
    _regs = regs vm
    newRegs = (V.//) _regs [(regIdx, value)]

withAddToReg :: Int -> Word8 -> VM -> VM
withAddToReg regIdx inc vm = vm {regs = newRegs}
  where
    _regs = regs vm
    oldValue = (V.!) _regs regIdx
    newRegs = (V.//) _regs [(regIdx, oldValue + inc)]

withSetRegFromReg :: Int -> Int -> VM -> VM
withSetRegFromReg lhsRegIdx rhsRegIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    rhsRegVal = (V.!) _regs rhsRegIdx
    newRegs = (V.//) _regs [(lhsRegIdx, rhsRegVal)]

withOrTwoRegs :: Int -> Int -> VM -> VM
withOrTwoRegs lhsRegIdx rhsRegIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    lhsRegVal = (V.!) _regs lhsRegIdx
    rhsRegVal = (V.!) _regs rhsRegIdx
    newRegs = (V.//) _regs [(lhsRegIdx, (.|.) lhsRegVal rhsRegVal)]

withAndTwoRegs :: Int -> Int -> VM -> VM
withAndTwoRegs lhsRegIdx rhsRegIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    lhsRegVal = (V.!) _regs lhsRegIdx
    rhsRegVal = (V.!) _regs rhsRegIdx
    newRegs = (V.//) _regs [(lhsRegIdx, (.&.) lhsRegVal rhsRegVal)]

withXorTwoRegs :: Int -> Int -> VM -> VM
withXorTwoRegs lhsRegIdx rhsRegIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    lhsRegVal = (V.!) _regs lhsRegIdx
    rhsRegVal = (V.!) _regs rhsRegIdx
    newRegs = (V.//) _regs [(lhsRegIdx, xor lhsRegVal rhsRegVal)]

withAddRegsWithCarry :: Int -> Int -> VM -> VM
withAddRegsWithCarry lhsRegIdx rhsRegIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    lhsRegVal = (V.!) _regs lhsRegIdx
    rhsRegVal = (V.!) _regs rhsRegIdx
    newVF = if (255 :: Word8) - lhsRegVal < rhsRegVal then 1 else 0
    newRegs = (V.//) _regs [(lhsRegIdx, lhsRegVal + rhsRegVal), (0xF, newVF)]

withSubRegsWithBorrow :: Int -> Int -> VM -> VM
withSubRegsWithBorrow lhsRegIdx rhsRegIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    lhsRegVal = (V.!) _regs lhsRegIdx
    rhsRegVal = (V.!) _regs rhsRegIdx
    newVF = if lhsRegVal > rhsRegVal then 1 else 0
    newRegs = (V.//) _regs [(lhsRegIdx, lhsRegVal - rhsRegVal), (0xF, newVF)]

withSubRegsReverseWithBorrow :: Int -> Int -> VM -> VM
withSubRegsReverseWithBorrow lhsRegIdx rhsRegIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    lhsRegVal = (V.!) _regs lhsRegIdx
    rhsRegVal = (V.!) _regs rhsRegIdx
    newVF = if rhsRegVal > lhsRegVal then 1 else 0
    newRegs = (V.//) _regs [(lhsRegIdx, rhsRegVal - lhsRegVal), (0xF, newVF)]

withShiftRightReg :: Int -> VM -> VM
withShiftRightReg regIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    regVal = (V.!) _regs regIdx
    newVF = (.&.) regVal 1
    newRegs = (V.//) _regs [(regIdx, shiftR regVal 1), (0xF, newVF)]

withShiftLeftReg :: Int -> VM -> VM
withShiftLeftReg regIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    regVal = (V.!) _regs regIdx
    newVF = (.&.) regVal 0x80
    newRegs = (V.//) _regs [(regIdx, shiftL regVal 1), (0xF, newVF)]

withRegISet :: Word16 -> VM -> VM
withRegISet value vm = vm {iReg = value}

withJumpValPlusV0 :: Word16 -> VM -> VM
withJumpValPlusV0 value vm = vm {pc = newPC}
  where
    _pc = pc vm
    _regs = regs vm
    v0 = (V.!) _regs 0
    newPC = (fromIntegral v0 :: Word16) + value

withRandomReg :: Int -> Word8 -> VM -> VM
withRandomReg = error "Not implemented (withRandomReg)"

withDrawSprite :: Int -> Int -> Int -> VM -> VM
withDrawSprite x y n vm = vm {displayBuffer = newDisplayBuffer, regs = newRegs}
  where
    _iReg = fromIntegral $ iReg vm
    _memory = memory vm
    _displayBuffer = displayBuffer vm
    _regs = regs vm
    xCoord = fromIntegral $ (V.!) _regs x
    yCoord = fromIntegral $ (V.!) _regs y
    -- N bytes.
    newBytes = (V.!) _memory . (+ _iReg) <$> [0 .. (n - 1)]
    newBits = Prelude.concatMap byteToBits newBytes
    displayIndices = Prelude.concatMap (\v -> (+ (displayWidth * mod (yCoord + v) displayHeight)) . flip mod displayWidth . (+ xCoord) <$> [0 .. 7]) [0 .. (n - 1)]
    currentDiplayBits = (V.!) _displayBuffer <$> displayIndices
    currentDisplayBytes = bitsToByte <$> chunksOf 8 currentDiplayBits
    didErase = Prelude.any (\(a, b) -> (.&.) a b > 0) $ Prelude.zip newBytes currentDisplayBytes
    newRegs = (V.//) _regs [(0xF, if didErase then 1 else 0)]
    newDisplayBuffer = (V.//) _displayBuffer $ Prelude.zip displayIndices newBits

-- currentBytes = (V.!) _memory

withSetRegFromTimer :: Int -> VM -> VM
withSetRegFromTimer regIdx vm = vm {regs = newRegs}
  where
    _regs = regs vm
    _timerReg = timerReg vm
    newRegs = (V.//) _regs [(regIdx, _timerReg)]

withSetTimerFromReg :: Int -> VM -> VM
withSetTimerFromReg regIdx vm = vm {timerReg = newTimerReg}
  where
    _regs = regs vm
    newTimerReg = (V.!) _regs regIdx

withSetSoundFromReg :: Int -> VM -> VM
withSetSoundFromReg regIdx vm = vm {soundReg = newSoundReg}
  where
    _regs = regs vm
    newSoundReg = (V.!) _regs regIdx

withAddRegToRegI :: Int -> VM -> VM
withAddRegToRegI regIdx vm = vm {iReg = newIReg}
  where
    _iReg = iReg vm
    _regs = regs vm
    value = (fromIntegral $ (V.!) _regs regIdx) :: Word16
    newIReg = value + _iReg

withSetIToSpriteLocation :: Int -> VM -> VM
withSetIToSpriteLocation regIdx vm = vm {iReg = newIReg}
  where
    _regs = regs vm
    value = (fromIntegral $ (V.!) _regs regIdx) :: Word16
    newIReg = assert (value <= 0xF) value * 5

withSetDecimalDigitsOfReg :: Int -> VM -> VM
withSetDecimalDigitsOfReg regIdx vm = vm {memory = newMemory}
  where
    _memory = memory vm
    _regs = regs vm
    _iReg = fromIntegral $ iReg vm
    value = (V.!) _regs regIdx
    hundreds = div value 100
    tens = div (mod value 100) 10
    ones = mod value 10
    newMemory = (V.//) _memory [(_iReg, hundreds :: Word8), (_iReg + 1, tens :: Word8), (_iReg + 2, ones :: Word8)]

withSaveRegsToMemory :: Int -> VM -> VM
withSaveRegsToMemory regIdx vm = vm {memory = newMemory}
  where
    _memory = memory vm
    _regs = fromIntegral <$> regs vm
    _iReg = fromIntegral $ iReg vm
    changeset = Prelude.zip ((+ _iReg) <$> [0 .. (regIdx - 1)]) (toList _regs)
    newMemory = (V.//) _memory changeset

withSaveMemoryToRegs :: Int -> VM -> VM
withSaveMemoryToRegs regIdx vm = vm {regs = newRegs}
  where
    _iReg = fromIntegral $ iReg vm
    _memory = memory vm
    _regs = regs vm
    range = [0 .. (regIdx - 1)]
    values = (V.!) _memory <$> ((+ _iReg) <$> range)
    changeset = Prelude.zip range values
    newRegs = (V.//) _regs changeset

-- Chip-8 provides 2 timers, a delay timer and a sound timer.
-- The delay timer is active whenever the delay timer register (DT) is non-zero. This timer does nothing more than subtract 1 from the value of DT at a rate of 60Hz. When DT reaches 0, it deactivates.
-- The sound timer is active whenever the sound timer register (ST) is non-zero. This timer also decrements at a rate of 60Hz, however, as long as ST's value is greater than zero, the Chip-8 buzzer will sound. When ST reaches zero, the sound timer deactivates.
-- The sound produced by the Chip-8 interpreter has only one tone. The frequency of this tone is decided by the author of the interpreter.
updateTimers :: VM -> VM
updateTimers = id -- TODO: Implement

updateInstruction :: VM -> VM
updateInstruction vm
  -- 00E0 - CLS
  -- Clear the display.
  | opCodeWord == 0x00E0 = withClearedDisplay . withPCInc $ vm
  -- 00EE - RET
  -- Return from a subroutine.
  -- The interpreter sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer.
  | opCodeWord == 0x00EE = withReturnFromStack vm
  -- 0nnn - SYS addr
  -- Jump to a machine code routine at nnn.
  -- This instruction is only used on the old computers on which Chip-8 was originally implemented. It is ignored by modern interpreters.
  | opCodeHiNibHi == 0 = withPC opCodeLo3Nibs vm
  -- 1nnn - JP addr
  -- Jump to location nnn.
  -- The interpreter sets the program counter to nnn.
  | opCodeHiNibHi == 1 = withPC opCodeLo3Nibs vm
  -- 2nnn - CALL addr
  -- Call subroutine at nnn.
  -- The interpreter increments the stack pointer, then puts the current PC on the top of the stack. The PC is then set to nnn.
  | opCodeHiNibHi == 2 = withCall opCodeLo3Nibs . withPCInc $ vm
  -- 3xkk - SE Vx, byte
  -- Skip next instruction if Vx = kk.
  -- The interpreter compares register Vx to kk, and if they are equal, increments the program counter by 2.
  | opCodeHiNibHi == 3 = withSkipInstructionIfRegEqual (fromIntegral opCodeHiNibLo) opCodeLo vm
  -- 4xkk - SNE Vx, byte
  -- Skip next instruction if Vx != kk.
  -- The interpreter compares register Vx to kk, and if they are not equal, increments the program counter by 2.
  | opCodeHiNibHi == 4 = withSkipInstructionIfRegNotEqual (fromIntegral opCodeHiNibLo) opCodeLo vm
  -- 5xy0 - SE Vx, Vy
  -- Skip next instruction if Vx = Vy.
  -- The interpreter compares register Vx to register Vy, and if they are equal, increments the program counter by 2.
  | opCodeHiNibHi == 5 && opCodeLoNibLo == 0 = withSkipInstructionIfTwoRegsEqual (fromIntegral opCodeLoNibHi) (fromIntegral opCodeHiNibLo) vm
  -- 6xkk - LD Vx, byte
  -- Set Vx = kk.
  -- The interpreter puts the value kk into register Vx.
  | opCodeHiNibHi == 6 = withLoadValueToReg (fromIntegral opCodeHiNibLo) opCodeLo . withPCInc $ vm
  -- 7xkk - ADD Vx, byte
  -- Set Vx = Vx + kk.
  -- Adds the value kk to the value of register Vx, then stores the result in Vx.
  | opCodeHiNibHi == 7 = withAddToReg (fromIntegral opCodeHiNibLo) opCodeLo . withPCInc $ vm
  -- 8xy0 - LD Vx, Vy
  -- Set Vx = Vy.
  -- Stores the value of register Vy in register Vx.
  | opCodeHiNibHi == 8 && opCodeLoNibLo == 0 = withSetRegFromReg (fromIntegral opCodeHiNibLo) (fromIntegral opCodeLoNibHi) . withPCInc $ vm
  -- 8xy1 - OR Vx, Vy
  -- Set Vx = Vx OR Vy.
  -- Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx. A bitwise OR compares the corrseponding bits from two values, and if either bit is 1, then the same bit in the result is also 1. Otherwise, it is 0.
  | opCodeHiNibHi == 8 && opCodeLoNibLo == 1 = withOrTwoRegs (fromIntegral opCodeHiNibLo) (fromIntegral opCodeLoNibHi) . withPCInc $ vm
  -- 8xy2 - AND Vx, Vy
  -- Set Vx = Vx AND Vy.
  -- Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx. A bitwise AND compares the corrseponding bits from two values, and if both bits are 1, then the same bit in the result is also 1. Otherwise, it is 0.
  | opCodeHiNibHi == 8 && opCodeLoNibLo == 2 = withAndTwoRegs (fromIntegral opCodeHiNibLo) (fromIntegral opCodeLoNibHi) . withPCInc $ vm
  -- 8xy3 - XOR Vx, Vy
  -- Set Vx = Vx XOR Vy.
  -- Performs a bitwise exclusive OR on the values of Vx and Vy, then stores the result in Vx. An exclusive OR compares the corrseponding bits from two values, and if the bits are not both the same, then the corresponding bit in the result is set to 1. Otherwise, it is 0.
  | opCodeHiNibHi == 8 && opCodeLoNibLo == 3 = withXorTwoRegs (fromIntegral opCodeHiNibLo) (fromIntegral opCodeLoNibHi) . withPCInc $ vm
  -- 8xy4 - ADD Vx, Vy
  -- Set Vx = Vx + Vy, set VF = carry.
  -- The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the lowest 8 bits of the result are kept, and stored in Vx.
  | opCodeHiNibHi == 8 && opCodeLoNibLo == 4 = withAddRegsWithCarry (fromIntegral opCodeHiNibLo) (fromIntegral opCodeLoNibHi) . withPCInc $ vm
  -- 8xy5 - SUB Vx, Vy
  -- Set Vx = Vx - Vy, set VF = NOT borrow.
  -- If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx, and the results stored in Vx.
  | opCodeHiNibHi == 8 && opCodeLoNibLo == 5 = withSubRegsWithBorrow (fromIntegral opCodeHiNibLo) (fromIntegral opCodeLoNibHi) . withPCInc $ vm
  -- 8xy6 - SHR Vx {, Vy}
  -- Set Vx = Vx SHR 1.
  -- If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2.
  | opCodeHiNibHi == 8 && opCodeLoNibLo == 6 = withShiftRightReg (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  -- 8xy7 - SUBN Vx, Vy
  -- Set Vx = Vy - Vx, set VF = NOT borrow.
  -- If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy, and the results stored in Vx.
  | opCodeHiNibHi == 8 && opCodeLoNibLo == 7 = withSubRegsReverseWithBorrow (fromIntegral opCodeHiNibLo) (fromIntegral opCodeLoNibHi) . withPCInc $ vm
  -- 8xyE - SHL Vx {, Vy}
  -- Set Vx = Vx SHL 1.
  -- If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2.
  | opCodeHiNibHi == 8 && opCodeLoNibLo == 0xE = withShiftLeftReg (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  -- 9xy0 - SNE Vx, Vy
  -- Skip next instruction if Vx != Vy.
  -- The values of Vx and Vy are compared, and if they are not equal, the program counter is increased by 2.
  | opCodeHiNibHi == 9 && opCodeLoNibLo == 0 = withSkipInstructionIfTwoRegsNotEqual (fromIntegral opCodeHiNibLo) (fromIntegral opCodeLoNibHi) vm
  -- Annn - LD I, addr
  -- Set I = nnn.
  -- The value of register I is set to nnn.
  | opCodeHiNibHi == 0xA = withRegISet opCodeLo3Nibs . withPCInc $ vm
  -- Bnnn - JP V0, addr
  -- Jump to location nnn + V0.
  -- The program counter is set to nnn plus the value of V0.
  | opCodeHiNibHi == 0xB = withJumpValPlusV0 opCodeLo3Nibs vm
  -- Cxkk - RND Vx, byte
  -- Set Vx = random byte AND kk.
  -- The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk. The results are stored in Vx. See instruction 8xy2 for more information on AND.
  | opCodeHiNibHi == 0xC = withRandomReg (fromIntegral opCodeHiNibLo) opCodeLo . withPCInc $ vm
  -- Dxyn - DRW Vx, Vy, nibble
  -- Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
  -- The interpreter reads n bytes from memory, starting at the address stored in I.
  -- These bytes are then displayed as sprites on screen at coordinates (Vx, Vy). Sprites are XORed onto the existing screen.
  -- If this causes any pixels to be erased, VF is set to 1, otherwise it is set to 0.
  -- If the sprite is positioned so part of it is outside the coordinates of the display, it wraps around to the opposite side of the screen.
  -- See instruction 8xy3 for more information on XOR, and section 2.4, Display, for more information on the Chip-8 screen and sprites.
  | opCodeHiNibHi == 0xD = withDrawSprite (fromIntegral opCodeHiNibLo) (fromIntegral opCodeLoNibHi) (fromIntegral opCodeLoNibLo) . withPCInc $ vm
  -- Ex9E - SKP Vx
  -- Skip next instruction if key with the value of Vx is pressed.
  -- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the down position, PC is increased by 2.
  | opCodeHiNibHi == 0xE && opCodeLo == 0x9E = error "Not implemented (Ex9E - SKP Vx)"
  -- ExA1 - SKNP Vx
  -- Skip next instruction if key with the value of Vx is not pressed.
  -- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the up position, PC is increased by 2.
  | opCodeHiNibHi == 0xE && opCodeLo == 0xA1 = error "Not implemented (ExA1 - SKNP Vx)"
  -- Fx07 - LD Vx, DT
  -- Set Vx = delay timer value.
  -- The value of DT is placed into Vx.
  | opCodeHiNibHi == 0xF && opCodeLo == 0x07 = withSetRegFromTimer (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  -- Fx0A - LD Vx, K
  -- Wait for a key press, store the value of the key in Vx.
  -- All execution stops until a key is pressed, then the value of that key is stored in Vx.
  | opCodeHiNibHi == 0xF && opCodeLo == 0x0A = error "Not implemented (Fx0A - LD Vx, K)"
  -- Fx15 - LD DT, Vx
  -- Set delay timer = Vx.
  -- DT is set equal to the value of Vx.
  | opCodeHiNibHi == 0xF && opCodeLo == 0x15 = withSetTimerFromReg (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  -- Fx18 - LD ST, Vx
  -- Set sound timer = Vx.
  -- ST is set equal to the value of Vx.
  | opCodeHiNibHi == 0xF && opCodeLo == 0x18 = withSetSoundFromReg (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  -- Fx1E - ADD I, Vx
  -- Set I = I + Vx.
  -- The values of I and Vx are added, and the results are stored in I.
  | opCodeHiNibHi == 0xF && opCodeLo == 0x1E = withAddRegToRegI (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  -- Fx29 - LD F, Vx
  -- Set I = location of sprite for digit Vx.
  -- The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx. See section 2.4,
  -- Display, for more information on the Chip-8 hexadecimal font.
  | opCodeHiNibHi == 0xF && opCodeLo == 0x29 = withSetIToSpriteLocation (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  -- Fx33 - LD B, Vx
  -- Store BCD representation of Vx in memory locations I, I+1, and I+2.
  -- The interpreter takes the decimal value of Vx, and places the hundreds digit in memory at location in I,
  -- the tens digit at location I+1, and the ones digit at location I+2.
  | opCodeHiNibHi == 0xF && opCodeLo == 0x33 = withSetDecimalDigitsOfReg (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  -- Fx55 - LD [I], Vx
  -- Store registers V0 through Vx in memory starting at location I.
  -- The interpreter copies the values of registers V0 through Vx into memory, starting at the address in I.
  | opCodeHiNibHi == 0xF && opCodeLo == 0x55 = withSaveRegsToMemory (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  -- Fx65 - LD Vx, [I]
  -- Read registers V0 through Vx from memory starting at location I.
  -- The interpreter reads values from memory starting at location I into registers V0 through Vx.
  | opCodeHiNibHi == 0xF && opCodeLo == 0x65 = withSaveMemoryToRegs (fromIntegral opCodeHiNibLo) . withPCInc $ vm
  | otherwise = trace ("Opcode not implemented: " Prelude.++ show opCodeWord) vm
  where
    (opCodeHi, opCodeLo) = traceOpcode (pc vm) $ opCode vm
    opCodeHiNibHi = shiftR opCodeHi 4 --  0xX...
    opCodeHiNibLo = (.&.) opCodeHi 0xF -- 0x.X..
    opCodeLoNibHi = shiftR opCodeLo 4 --  0x..X.
    opCodeLoNibLo = (.&.) opCodeLo 0xF -- 0x...X
    opCodeWord = (.|.) (shiftL (fromIntegral opCodeHi :: Word16) 8) (fromIntegral opCodeLo :: Word16)
    opCodeLo3Nibs = (.&.) opCodeWord 0x0FFF -- 0x.XXX
