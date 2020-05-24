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
import Data.Map
import qualified SDL
import qualified SDL.Font as SFont
import FRP.Yampa.Delays

type BallSF = SF GameInput GameOutput

type WinInput = Event SDL.Event

data Texture = Texture {
  texture :: SDL.Texture,
  size :: V2 CInt
}

data Ball = Ball {
  position :: Point V2 Double,
  velocity :: V2 Double,
  acceleration :: V2 Double,
  radius :: Double,
  power :: Double
}

data GameObjType = Player | Wall | Spikes | Goal deriving Eq

data GameObj = GameObj {
  objType :: GameObjType,
  textureCoords :: SDL.Rectangle CInt
}

type GidsToObjsMap = Map Word32 GameObj -- map from tilemap Gids to game objects with corresponding textures

type StaticObjsMap = Map (Int, Int) GameObj -- map from positions to gameobjs which should be drawn in corresponding chunks

data GameOutput = GameOutput {
  ball :: Ball,
  shouldEnd :: Bool,
  currTimeOut :: Integer,
  nShots :: Int,
  didWin :: Bool
}

data GameInfo = GameInfo {
  screenWidth :: CInt,
  screenHeight :: CInt,
  tileWidth :: CInt,
  tileHeight :: CInt,
  objsMap :: StaticObjsMap
}

data GameInput = GameInput {
  mPressed :: Bool, -- checks whether LMB is pressed
  mEventPressed :: Event (), -- fires when LMB is pressed
  mEventReleased :: Event (), -- fires when LMB is released
  mPos :: Point V2 CInt,
  qClick :: Event (),
  mClick :: Event (),
  currTimeIn :: Integer
}

data CollisionEffect = CollisionEffect {
  leftHitEffect :: Maybe GameObjType,
  rightHitEffect :: Maybe GameObjType,
  upHitEffect :: Maybe GameObjType,
  downHitEffect :: Maybe GameObjType,
  leftUpHitEffect :: Maybe GameObjType,
  rightUpHitEffect :: Maybe GameObjType,
  leftDownHitEffect :: Maybe GameObjType,
  rightDownHitEffect :: Maybe GameObjType
}

type HardSF = SF (GameInput, Point V2 Double, V2 Double, V2 Double, Bool) (V2 Double, V2 Double)

data HitDir = LeftSide | RightSide | UpSide | DownSide | LeftUpSide | RightUpSide | LeftDownSide | RightDownSide