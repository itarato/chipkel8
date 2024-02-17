module Main where

import Config
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Word
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import VM (VM, initVM, updateInstruction, updateTimers)

{-

Memory Map:
+---------------+= 0xFFF (4095) End of Chip-8 RAM
\|               |
\|               |
\|               |
\|               |
\|               |
\| 0x200 to 0xFFF|
\|     Chip-8    |
\| Program / Data|
\|     Space     |
\|               |
\|               |
\|               |
+- - - - - - - -+= 0x600 (1536) Start of ETI 660 Chip-8 programs
\|               |
\|               |
\|               |
+---------------+= 0x200 (512) Start of most Chip-8 programs
\| 0x000 to 0x1FF|
\| Reserved for  |
\|  interpreter  |
+---------------+= 0x000 (0) Start of Chip-8 RAM

-}

data Appx = MakeAppx {vmx :: VM, inputs :: V.Vector Bool}

initAppx :: [Word8] -> Appx
initAppx _program = MakeAppx {vmx = initVM _program, inputs = V.replicate 16 False}

updateAppx :: Float -> Appx -> Appx
updateAppx _elapsed apps = apps {vmx = newVm}
  where
    _vm = (trace "dcon" (vmx apps))
    newVm = updateTimers . updateInstruction $ (trace "vm" _vm)

window :: Display
window = InWindow "Chip8" (displayWidth * pixelSize, displayHeight * pixelSize) (100, 100)

render :: Appx -> Picture
render _app = pictures []

handleEvent :: Event -> Appx -> Appx
handleEvent _event appa = appa

main :: IO ()
main = do
  _program <- BS.unpack <$> BS.readFile "roms/spaceinvader.ch8"
  play window black 60 (initAppx _program) render handleEvent updateAppx
