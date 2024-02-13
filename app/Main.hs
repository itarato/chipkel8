module Main where

import Config
import Control.Exception
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import Data.List.Extra
import Data.Vector
import qualified Data.Vector as V
import Data.Word
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

updateVM :: Float -> VM -> VM
updateVM elapsed = updateTimers . updateInstruction

window :: Display
window = InWindow "Chip8" (displayWidth * pixelSize, displayHeight * pixelSize) (100, 100)

render :: VM -> Picture
render vm = pictures []

handleEvent :: Event -> VM -> VM
handleEvent event vm = vm

main :: IO ()
main = do
  _program <- BS.unpack <$> BS.readFile "roms/spaceinvader.ch8"
  play window black 60 (initVM _program) render handleEvent updateVM
