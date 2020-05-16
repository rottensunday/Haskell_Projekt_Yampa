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

type BallSF = SF GameInput Ball

type WinInput = Event SDL.Event

data Ball = Ball {
  position :: Point V2 Double,
  velocity :: V2 Double,
  acceleration :: V2 Double,
  radius :: Double
}

data GameInfo = GameInfo {
    screenWidth :: CInt,
    screenHeight :: CInt,
    tileWidth :: CInt,
    tileHeight :: CInt
}

data GameInput = GameInput {
  mPos :: Point V2 CInt,
  mClick :: Event ()
}

type HardSF = SF (GameInput, Point V2 Double, V2 Double, V2 Double, Bool) (V2 Double, V2 Double)

data HitDir = LeftSide | RightSide | UpSide | DownSide