module Main where

import Config
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Word
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import VM (VM, initVM, sp, updateInstruction, updateTimers)

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

data App = MakeApp {vm :: VM, input :: V.Vector Bool}

initApp :: [Word8] -> App
initApp _program = MakeApp {vm = initVM _program, input = V.replicate 16 False}

updateApp :: Float -> App -> App
updateApp _elapsed apps = apps {vm = newVm}
  where
    newVm = updateTimers . updateInstruction $ vm apps

window :: Display
window = InWindow "Chip8" (displayWidth * pixelSize, displayHeight * pixelSize) (100, 100)

render :: App -> Picture
render app = pictures []

handleEvent :: Event -> App -> App
handleEvent _event appa = appa

main :: IO ()
main = do
  _program <- BS.unpack <$> BS.readFile "roms/spaceinvader.ch8"
  play window black 60 (initApp _program) render handleEvent updateApp
