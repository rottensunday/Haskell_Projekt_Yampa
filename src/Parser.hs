{-|
Module      : Parser
Description : Parse input
-}
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
import Data.Time.Clock.POSIX (getPOSIXTime)

import Types

-- |Given GameInput, we get mouse position as a 2D Point
mousePosParser :: SF GameInput (Point V2 Double)
mousePosParser = proc gi -> do
  doublePos <- intToDoubleConverter -< mPos gi
  returnA -< doublePos

intToDoubleConverter :: SF (Point V2 CInt) (Point V2 Double)
intToDoubleConverter = proc gi@(P (V2 px py)) -> do
  posX <- arr fromIntegral -< px
  posY <- arr fromIntegral -< py
  returnA -< P (V2 posX posY)

-- |Given GameInput, we check whether Q button was clicked
qClickParser :: SF GameInput (Event ())
qClickParser = arr qClick

-- |Given GameInput, we check whether LMB is pressed
mousePressedParser :: SF GameInput Bool
mousePressedParser = arr mPressed

-- |Given GameInput, we check whether LMB was just pressed
mouseEventPressedParser :: SF GameInput (Event ())
mouseEventPressedParser = arr mEventPressed

-- |Given GameInput, we check whether LMB was just released
mouseEventReleasedParser :: SF GameInput (Event ())
mouseEventReleasedParser = arr mEventReleased

-- |Given GameInput, we get input time
timeParser :: SF GameInput Integer
timeParser = arr currTimeIn
  
-- |Using SDL, we get all important inputs from user and create GameInput object
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
  inTime <- round . (*1000000) <$> getPOSIXTime -- POSIX Time in microseconds
  return GameInput { mPressed = checkBtn SDL.ButtonLeft, 
                    mEventPressed = if lmbPressed then Event () else NoEvent,
                    mEventReleased = if lmbReleased then Event () else NoEvent,
                    mPos = mousePos, 
                    qClick = if qPressed then Event () else NoEvent,
                    currTimeIn = inTime } 