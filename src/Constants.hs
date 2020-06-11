{-|
Module      : Constants
Description : Magic numbers go here
-}
module Constants where

import Linear
import Foreign.C.Types
import qualified SDL

import Types

initScreenWidth, initScreenHeight :: CInt
(initScreenWidth, initScreenHeight) = (1280, 960)

initTileWidth, initTileHeight :: CInt
(initTileWidth, initTileHeight) = (32, 32)

spriteSheetPath = "../resources/sheep3.bmp"
mapsPath = "../resources/levels/"
fontPath = "../resources/arial.ttf"

windowName = "Ball Game!"

spriteSize = V2 initTileWidth initTileHeight
ballClip = SDL.Rectangle (SDL.P (V2 0 0)) spriteSize
wallClip = SDL.Rectangle (SDL.P (V2 initTileWidth 0)) spriteSize
spikeClip = SDL.Rectangle (SDL.P (V2 0 initTileHeight)) spriteSize
goalClip = SDL.Rectangle (SDL.P (V2 initTileWidth initTileHeight)) spriteSize

startBall = Ball {
  position = SDL.P (V2 657 (fromIntegral initTileHeight)),
  velocity = V2 0 0,
  acceleration = V2 0 45,
  radius = 13,
  power = 60
}

simulationRate = 0.015 :: Double