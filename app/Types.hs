module Types where

import FRP.Yampa
import qualified Data.VectorSpace as DD
import Linear
import Linear.Metric
import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL (($=), Point(..), Rectangle)
import Data.Word
import Data.Text(Text(..), pack)
import Data.Vector3
import qualified SDL
import qualified SDL.Font as SFont
import FRP.Yampa.Delays

type BallSF = SF GameInput GameOutput

type WinInput = Event SDL.Event

data Ball = Ball {
  position :: Point V2 Double,
  velocity :: V2 Double,
  acceleration :: V2 Double,
  radius :: Double
}

data GameOutput = GameOutput {
  ball :: Ball,
  shouldEnd :: Bool
}

data GameInfo = GameInfo {
  screenWidth :: CInt,
  screenHeight :: CInt,
  tileWidth :: CInt,
  tileHeight :: CInt
}

data GameInput = GameInput {
  mPressed :: Bool, -- checks whether LMB is pressed
  mEventPressed :: Event (), -- fires when LMB is pressed
  mEventReleased :: Event (), -- fires when LMB is released
  mPos :: Point V2 CInt,
  qClick :: Event (),
  mClick :: Event ()
}

type HardSF = SF (GameInput, Point V2 Double, V2 Double, V2 Double, Bool) (V2 Double, V2 Double)

data HitDir = LeftSide | RightSide | UpSide | DownSide