module Config where

import Data.Vector

type Input = Vector Bool

fps :: Int
fps = 30

displayWidth :: Int
displayWidth = 64

displayHeight :: Int
displayHeight = 32

displayBufferSize :: Int
displayBufferSize = displayWidth * displayHeight

pixelSize :: Int
pixelSize = 4

pixelSizeF :: Float
pixelSizeF = fromIntegral pixelSize

instructionPerBatch :: Int
instructionPerBatch = 24
