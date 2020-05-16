{-# LANGUAGE Arrows #-}
module Parser where

import FRP.Yampa
import Linear
import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL (($=), Point(..), Rectangle)
import qualified SDL
import GHC.IO.Encoding

import Types

mousePosParser :: SF GameInput (Point V2 Double)
mousePosParser = proc gi -> do
  doublePos <- intToDoubleConverter -< mPos gi
  returnA -< doublePos

intToDoubleConverter :: SF (Point V2 CInt) (Point V2 Double)
intToDoubleConverter = proc gi@(P (V2 px py)) -> do
  posX <- arr fromIntegral -< px
  posY <- arr fromIntegral -< py
  returnA -< P (V2 posX posY)

mouseClickParser :: SF GameInput (Event ())
mouseClickParser = proc gi -> do
  returnA -< mClick gi

parseInput :: IO GameInput
parseInput = do
  events <- SDL.pollEvents
  mousePos <- SDL.getAbsoluteMouseLocation
  let eventIsButtonPressed event =
        case SDL.eventPayload event of
          SDL.MouseButtonEvent buttonEvent ->
            SDL.mouseButtonEventMotion buttonEvent == SDL.Released &&
            SDL.mouseButtonEventButton buttonEvent == SDL.ButtonLeft
          _ -> False
      qPressed = any eventIsButtonPressed events

  return GameInput {mPos = mousePos, mClick = if qPressed then Event () else NoEvent }