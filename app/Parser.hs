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

qClickParser :: SF GameInput (Event ())
qClickParser = proc gi -> do
  returnA -< qClick gi

mousePressedParser :: SF GameInput Bool
mousePressedParser = proc gi -> do
  returnA -< mPressed gi

mouseEventPressedParser :: SF GameInput (Event ())
mouseEventPressedParser = proc gi -> do
  returnA -< mEventPressed gi

mouseEventReleasedParser :: SF GameInput (Event ())
mouseEventReleasedParser = proc gi -> do
  returnA -< mEventReleased gi
  

parseInput :: IO GameInput
parseInput = do
  events <- SDL.pollEvents
  mousePos <- SDL.getAbsoluteMouseLocation
  checkBtn <- SDL.getMouseButtons
  let eventIsButtonPressed event =
        case SDL.eventPayload event of
          SDL.MouseButtonEvent buttonEvent ->
            SDL.mouseButtonEventMotion buttonEvent == SDL.Pressed &&
            SDL.mouseButtonEventButton buttonEvent == SDL.ButtonLeft
          _ -> False
      eventIsButtonReleased event = 
        case SDL.eventPayload event of
          SDL.MouseButtonEvent buttonEvent ->
            SDL.mouseButtonEventMotion buttonEvent == SDL.Released &&
            SDL.mouseButtonEventButton buttonEvent == SDL.ButtonLeft
          _ -> False
      eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
      lmbPressed = any eventIsButtonPressed events
      lmbReleased = any eventIsButtonReleased events

  return GameInput { mPressed = checkBtn SDL.ButtonLeft, 
                    mEventPressed = if lmbPressed then Event () else NoEvent,
                    mEventReleased = if lmbReleased then Event () else NoEvent,
                    mPos = mousePos, 
                    qClick = if qPressed then Event () else NoEvent,
                    mClick = if lmbReleased then Event () else NoEvent } -- this is legacy option