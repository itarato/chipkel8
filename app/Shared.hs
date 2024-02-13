module Shared where

import Data.Bits
import Data.Word

byteToBits :: Word8 -> [Word8]
byteToBits byte = (\v -> if testBit byte v then 1 else 0) <$> [7, 6 .. 0]

bitsToByte :: [Word8] -> Word8
bitsToByte bits = Prelude.sum $ (\(b, i) -> b * bit i) <$> Prelude.zip bits [7, 6 .. 0]
