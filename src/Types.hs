{-|
Module      : Types
Description : Types definitions
-}
module Types where

import FRP.Yampa
import Linear
import Linear.Affine
import Foreign.C.Types
import Data.Word
import Data.Map
import qualified SDL

type GameSF = SF GameInput GameOutput

-- type WinInput = Event SDL.Event

-- |Simple wrapper around SDL Texture which also contains texture size
data Texture = Texture {
  texture :: SDL.Texture,     -- ^ SDL Texture
  size :: V2 CInt             -- ^ Texture size
}

-- |Necessary Ball information
data Ball = Ball {
  position :: Point V2 Double,      -- ^ Current Ball position
  velocity :: V2 Double,            -- ^ Current Ball velocity
  acceleration :: V2 Double,        -- ^ Current Ball acceleration
  radius :: Double,                 -- ^ Ball radius
  power :: Double                   -- ^ Power controls how fast our shot power loads
}

-- |Type of Game Object
data GameObjType = Player | Wall | Spikes | Goal deriving Eq

-- |GameObj contains necessary data to draw object: its type and texture coordinates in sheet
data GameObj = GameObj {
  objType :: GameObjType,
  textureCoords :: SDL.Rectangle CInt
}

-- |DynamicGameObj contains necessary data ato control dynamic object changes
data DynamicGameObj = DynamicGameObj {
  gameObj :: GameObj,             -- ^ Data needed to draw object
  startPos :: Point V2 Double,    -- ^ Starting position of object
  direction :: Direction,         -- ^ Direction of movement
  speed :: Double,                -- ^ Object speed
  limit :: Double                 -- ^ Maximum deviation from start position
}

-- |Map from tilemap Gids to game objects with corresponding textures
type GidsToObjsMap = Map Word32 GameObj

-- |Map from window positions to gameobjs which should be drawn in corresponding positions
type ObjsMap = Map (Int, Int) GameObj

-- |GameInput is given to main game controller
data GameInput = GameInput {
  mPressed :: Bool,               -- ^ Checks whether LMB is pressed
  mEventPressed :: Event (),      -- ^ Fires when LMB is pressed
  mEventReleased :: Event (),     -- ^ Fires when LMB is released
  mPos :: Point V2 CInt,          -- ^ Mouse position
  qClick :: Event (),             -- ^ Fires when Q is clicked (check whether game should end)
  currTimeIn :: Integer           -- ^ Passes start time to output
}

-- |GameOutput is returned by main game controller
data GameOutput = GameOutput {
  ball :: Ball,               -- ^ Output ball info
  shouldEnd :: Bool,          -- ^ Did we click Q? Then SDL should finish process
  nShots :: Int,              -- ^ Number of player shots
  didWin :: Bool,             -- ^ Did player finish lvl?
  didFinishGame :: Bool,      -- ^ Did player finish the whole game?
  outLvl :: Int,              -- ^ Simply pass current level to output  
  objsMap :: ObjsMap,         -- ^ Pass all game objects info
  currTimeOut :: Integer      -- ^ Pass start time to SDL
}

-- |GameInfo contains initial game data
data GameInfo = GameInfo {
  screenWidth :: CInt,                    -- ^ Window width in pixels
  screenHeight :: CInt,                   -- ^ Window height in pixels
  tileWidth :: CInt,                      -- ^ Tile width in pixels
  tileHeight :: CInt,                     -- ^ Tile height in pixels
  staticObjsMap :: [ObjsMap],             -- ^ Loaded static objects map for each map
  dynamicObjsInfo :: [[DynamicGameObj]],  -- ^ Loaded dynamic objects collection for each map
  currLvl :: Int                          -- ^ Starting level
}

-- |Used in collisions. Contains information about side we hit and type of object we collided
-- with on given side
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

-- |Which side of a rectangle did we hit?
data HitDir = LeftSide | RightSide | UpSide | DownSide | LeftUpSide | RightUpSide | LeftDownSide | RightDownSide

-- |Direction of dynamic object movement
data Direction = VerticalUp | VerticalDown | HorizontalLeft | HorizontalRight