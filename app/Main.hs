{-# LANGUAGE TupleSections #-}

module Main where

import Config
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import qualified Data.Vector as V
import Data.Word
import Debug.Trace (traceShowId)
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact
import System.Environment
import VM (VM (displayBuffer), initVM, updateInstructionBatch, updateTimers)

data App = MakeApp {vm :: VM, input :: Input}

initApp :: [Word8] -> App
initApp _program = MakeApp {vm = initVM _program, input = V.replicate 16 False}

updateApp :: Float -> App -> App
updateApp _elapsed app = app {vm = newVm}
  where
    _input = input app
    newVm = updateTimers . updateInstructionBatch instructionPerBatch _input $ vm app

window :: Int -> Int -> Display
window screenW screenH = InWindow "Chip8 (in Haskell)" (w, h) (posX, posY)
  where
    w = (displayWidth + 1) * pixelSize
    h = (displayHeight + 1) * pixelSize
    posX = div (screenW - w) 2
    posY = div (screenH - h) 2

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
    _x = (x - div displayWidth 2) * pixelSize + div pixelSize 2
    _y = (div displayHeight 2 - y) * pixelSize - div pixelSize 2

getKey :: Event -> Maybe (Char, Bool)
getKey (EventKey key keyState _ _)
  | (Char c) <- key, Down <- keyState = Just (c, True)
  | (Char c) <- key, Up <- keyState = Just (c, False)
getKey _ = Nothing

getKeyIdx :: Char -> Maybe Int
getKeyIdx c = case c of
  '1' -> Just 0
  '2' -> Just 1
  '3' -> Just 2
  '4' -> Just 3
  'q' -> Just 4
  'w' -> Just 5
  'e' -> Just 6
  'r' -> Just 7
  'a' -> Just 8
  's' -> Just 9
  'd' -> Just 10
  'f' -> Just 11
  'z' -> Just 12
  'x' -> Just 13
  'c' -> Just 14
  'v' -> Just 15
  _ -> Nothing

handleEvent :: Event -> App -> App
handleEvent event app = app {input = newInput}
  where
    maybeKey = getKey event
    keyIdx = (\(c, b) -> (,b) <$> getKeyIdx c) =<< maybeKey
    newInput = (V.//) (input app) $ maybeToList keyIdx

main :: IO ()
main = do
  args <- getArgs
  if null $ traceShowId args
    then print "Missing rom file name"
    else do
      (screenW, screenH) <- getScreenSize
      _program <- BS.unpack <$> BS.readFile (head args)
      play (window screenW screenH) black fps (initApp _program) render handleEvent updateApp
