module Main where

import Config
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import VM (VM (displayBuffer), initVM, updateInstruction, updateTimers)

data App = MakeApp {vm :: VM, input :: V.Vector Bool}

initApp :: [Word8] -> App
initApp _program = MakeApp {vm = initVM _program, input = V.replicate 16 False}

updateApp :: Float -> App -> App
updateApp _elapsed app = app {vm = newVm}
  where
    newVm = updateTimers . updateInstruction $ vm app

window :: Display
window = InWindow "Chip8" (displayWidth * pixelSize, displayHeight * pixelSize) (100, 100)

render :: App -> Picture
render app = pictures $ V.toList $ pictureOfPixelWithCoord <$> filteredCoordsAndPixels
  where
    _displayBuffer = displayBuffer $ vm app
    coords = (\i -> (mod i displayWidth, div i displayWidth)) <$> V.fromList [0 .. (displayWidth * displayHeight - 1)]
    coordsAndPixels = V.zip coords _displayBuffer
    filteredCoordsAndPixels = V.filter (\(_, v) -> v == 1) coordsAndPixels

pictureOfPixelWithCoord :: ((Int, Int), Word8) -> Picture
pictureOfPixelWithCoord ((x, y), _) = translate (fromIntegral _x) (fromIntegral _y) $ color white $ rectangleSolid pixelSizeF pixelSizeF
  where
    _x = (x - div displayWidth 2) * pixelSize
    _y = (div displayHeight 2 - y) * pixelSize

handleEvent :: Event -> App -> App
handleEvent _event = id

main :: IO ()
main = do
  _program <- BS.unpack <$> BS.readFile "roms/spaceinvader.ch8"
  play window black 60 (initApp _program) render handleEvent updateApp
