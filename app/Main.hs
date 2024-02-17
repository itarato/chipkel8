{-# LANGUAGE TupleSections #-}

module Main where

import Config
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import qualified Data.Vector as V
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import VM (VM (displayBuffer), initVM, updateInstructionBatch, updateTimers)

data App = MakeApp {vm :: VM, input :: Input}

initApp :: [Word8] -> App
initApp _program = MakeApp {vm = initVM _program, input = V.replicate 16 False}

updateApp :: Float -> App -> App
updateApp _elapsed app = app {vm = newVm}
  where
    _input = input app
    newVm = updateTimers . updateInstructionBatch instructionPerBatch _input $ vm app

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

getKey :: Event -> Maybe (Char, Bool)
getKey (EventKey key keyState _ _)
  | (Char c) <- key, Down <- keyState = Just (c, False)
  | (Char c) <- key, Up <- keyState = Just (c, True)
getKey _ = Nothing

getKeyIdx :: Char -> Maybe Int
getKeyIdx c = case c of
  '0' -> Just 0
  '1' -> Just 1
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  'a' -> Just 10
  'b' -> Just 11
  'c' -> Just 12
  'd' -> Just 13
  'e' -> Just 14
  'f' -> Just 15
  _ -> Nothing

handleEvent :: Event -> App -> App
handleEvent event app = app {input = newInput}
  where
    maybeKey = getKey event
    keyIdx = (\(c, b) -> (,b) <$> getKeyIdx c) =<< maybeKey
    newInput = (V.//) (input app) $ maybeToList keyIdx

main :: IO ()
main = do
  _program <- BS.unpack <$> BS.readFile "roms/spaceinvader.ch8"
  play window black 60 (initApp _program) render handleEvent updateApp
