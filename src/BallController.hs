{-|
Module      : BallController
Description : Brain of the game: ball signal function
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
module BallController where

import FRP.Yampa
import qualified Data.VectorSpace as DD
import Linear
import Linear.Metric
import Control.Monad
import Foreign.C.Types
import SDL (($=), Point(..), Rectangle)
import Data.Word
import Data.Text(Text(..), pack)
import Data.Vector3
import Data.Maybe
import qualified Data.Map as Map
import FRP.Yampa.Delays
import FRP.Yampa.Switches
import Control.Lens

import Parser
import Types
import Utility

-- |ballController is responsible for controlling ball state. 
-- It's one big SF which uses many other SFs defined in this module.
ballController :: Ball                                -- ^ Initial ball
              -> GameInfo                             -- ^ Initial game state
              -> SF (GameInput, ObjsMap) GameOutput
ballController b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r pow) gi = switch controller (const $ ballController b gi)
              where
                controller :: SF (GameInput, ObjsMap) (GameOutput, Event ())
                controller = proc inp@(input, objs) -> do
                  lmbReleased <- mouseEventReleasedParser -< input -- determine whether we released LMB
                  nClicks <- accumHold 0 -< lmbReleased `tag` (+1) -- count LMB clicks
                  qClicked <- qClickParser >>> arr isEvent -< input -- determine whether we clicked Q

                  -- we have to use delayed switch so jumpPower isn't set to 0 after LMB release. If this happens, we can't really use it to shoot our ball.
                  jumpPower <-
                      drSwitch (mousePressedParser >>> powerCalc pow 0) -<
                      (input, lmbReleased `tag` (mousePressedParser >>> powerCalc pow 0))
                  mPos <- mousePosParser -< input -- determine mouse position

                  rec velModClick <- clickSpeedModifier -< (lmbReleased `tag` (mPos, P (V2 posX posY), jumpPower)) -- value of speed to switch into on LMB releasse
                      -- hitting an edge may change our velocity and acceleration. If we hit a floor and our velocity is very low, we start rolling on the ground.
                      (collisionEvent, rolling) <- fby (NoEvent, False) (collisionChecker gi) -< (P (V2 posX posY), speed, r, objs)
                      velModHit <- hitSAModifier -< (collisionEvent `attach` (speed, acc, rolling))
                      -- every time we have click or edge hit event, we switch our speed-determining SF to a new one,
                      -- so it can make its calculation based on new parameters. We provide a parameter for this SF with a "getTag" function,
                      -- which processes click/edge hit events modified values and makes a new ball.
                      (speed@(V2 speedX speedY), acc@(V2 accX accY)) <-
                          fby (V2 0.0 0.0, V2 0.0 0.0) (rSwitch (getVA b)) -<
                          ((speed, acc, rolling),
                            lMerge lmbReleased (collisionEvent `tag` ())
                              `tag` getVA (getTag lmbReleased (collisionEvent `tag` ()) velModClick velModHit (P (V2 posX posY)) speed acc r rolling))
                      -- in next two lines we calculate ball position. It's just an integral over speed.
                      -- We switch to previous position on collision (we "back off" to position before collision)
                      posX <- rSwitch (iPre px >>> integral >>^ (+px)) -< (speedX, collisionEvent `tag` (integral >>^ (+((prevVals !! 2) ^. _x))))
                      posY <- rSwitch (iPre py >>> integral >>^ (+py)) -< (speedY, collisionEvent `tag` (integral >>^ (+((prevVals !! 2) ^. _y))))
                      eventLose <- verifyLoseCondition -< collisionEvent
                      eventWin <- verifyWinCondition -< collisionEvent
                      prevVals <- fby [V2 px py, V2 px py, V2 px py] computePrevVals -< (posX, posY, prevVals) -- here, we save previous position
                  retTime <- timeParser  -< input
                  returnA -< (GameOutput {
                              ball = Ball {
                                position = P (V2 posX posY),
                                velocity = speed,
                                acceleration = acc,
                                radius = r,
                                power = jumpPower
                              },
                              shouldEnd = qClicked,
                              currTimeOut = retTime,
                              nShots = nClicks,
                              didWin = isEvent eventWin,
                              didFinishGame = False,
                              outLvl = 0,
                              objsMap = objs
                          }, eventLose)
                -- getTag is responsible for generating new ball info (velocity and acceleration) on discrete events (LMB clicks and collisions)
                -- getTag (clickEvent) (hitEvent) (clickEventModifiers) (hitEventModifiers) (position) (speed) (acceleration) (radius) (isRolling)
                getTag :: Event () -> Event () -> V2 Double -> (V2 Double, V2 Double) -> Point V2 Double -> V2 Double -> V2 Double -> Double -> Bool -> Ball
                getTag NoEvent NoEvent _ _ _ _ _ _ _ = undefined -- this cannot be called
                getTag (Event ()) NoEvent modC@(V2 vx vy) modH p v a r rolling = Ball { -- after click
                                            position = p,
                                            velocity = if rolling && vy > 0 then V2 vx 0 else modC,
                                            acceleration = a,
                                            radius = r,
                                            power = 0
                                          }
                getTag NoEvent (Event ()) modC modH@(modHv, modHa) p v a r rolling = Ball { -- after collision
                                            position = p,
                                            velocity = modHv,
                                            acceleration = modHa,
                                            radius = r,
                                            power = 0
                                          }
                getTag (Event ()) (Event ()) modC modH@(modHv, modHa) p v a r rolling = Ball { -- this being called is highly unlikely
                                            position = p,
                                            velocity = modHv,
                                            acceleration = modHa,
                                            radius = r,
                                            power = 0
                                          }

-- |Our clicks should only modify velocity, based on ball position, player position and power given
clickSpeedModifier :: SF (Event (Point V2 Double, Point V2 Double, Double)) (V2 Double)
clickSpeedModifier = proc input -> do
  case input of
    NoEvent -> returnA -< 0
    Event (P mPos, P pPos, pow) ->
      returnA -< velChange
        where
          dir@(V2 vx vy) = Linear.Metric.normalize (mPos - pPos) -- determine direction of movement
          velChange = V2 pow pow * dir -- our new velocity vector has length of 50 in direction of mouse position

-- |hitSAModifier is a Hit Speed Acceleration Modifier. On collision we will change speed and acceleration.
-- For example, when we're rolling and friction works and we hit a wall, friction direction should change.
hitSAModifier :: SF (Event ((GameObjType, HitDir), (V2 Double, V2 Double, Bool))) (V2 Double, V2 Double)
hitSAModifier = proc input -> do
  case input of
    NoEvent -> returnA -< (V2 0 0, V2 0 0)

    Event ((Wall, UpSide), (v@(V2 vX vY), a@(V2 aX aY), rolling)) ->
      -- if we hit a ground and our velocity is very low, we should stop. If we hit a ground and are now rolling, we should apply friction.
      returnA -< (V2 vX (if abs vY <= 10 then 0 else if vY > 0 then vY * (-0.6) else vY),
                  if rolling then V2 (calculateXAcc vX) aY else a)

    Event ((Wall, DownSide), (v@(V2 vX vY), a@(V2 aX aY), rolling)) ->
      -- if we hit a roof, we just bounce off of it
      returnA -< (V2 vX (vY * (-0.6)), a)

    Event ((Wall, LeftSide), (v@(V2 vX vY), a@(V2 aX aY), rolling)) ->
      -- hitting left or right wall should change direction of X acceleration. This is used only for friction. 
      -- Addition of other horizontal forces may change this.
      returnA -< (V2 (vX * (-0.6)) vY, V2 (-aX) aY)

    Event ((Wall, RightSide), (v@(V2 vX vY), a@(V2 aX aY), rolling)) ->
      returnA -< (V2 (vX * (-0.6)) vY, V2 (-aX) aY)

    -- collision with corner should reverse both speed and acceleration. But we have to consider some edge cases.
    Event ((Wall, LeftDownSide), (v@(V2 vX vY), a@(V2 aX aY), rolling)) ->
      returnA -< if vX < 0 then (V2 vX (vY * (-0.6)), a) 
              else if vY > 0 then (V2 (-vX) vY, a)
              else (V2 (-vX) (vY * (-0.6)), a)

    Event ((Wall, RightDownSide), (v@(V2 vX vY), a@(V2 aX aY), rolling)) ->
      returnA -< if vX > 0 then (V2 vX (vY * (-0.6)), a) 
              else if vY > 0 then (V2 (-vX) vY, a)
              else (V2 (-vX) (vY * (-0.6)), a)

    Event ((Wall, LeftUpSide), (v@(V2 vX vY), a@(V2 aX aY), rolling)) ->
      returnA -< if rolling then (v, a) 
              else if vX < 0 then (V2 vX (vY * (-0.6)), a) 
              else if vY < 0 then (V2 (-vX) vY, a)
              else (V2 (-vX) (vY * (-0.6)), a)

    Event ((Wall, RightUpSide), (v@(V2 vX vY), a@(V2 aX aY), rolling)) ->
      returnA -< if rolling then (v, a) 
              else if vX > 0 then (V2 vX (vY * (-0.6)), a) 
              else if vY < 0 then (V2 (-vX) vY, a)
              else (V2 (-vX) (vY * (-0.6)), a)

  where calculateXAcc vX
          | vX <= 0   = 1.5
          | otherwise = -1.5

-- |powerCalc is supposed to calculate power of ball shot.
-- it just applies acc value to integral and switches, so we go back and forward
-- between 0 and 100 power value
powerCalc :: Double           -- ^ Speed of power change
        -> Double             -- ^ Initial power value (used for switching)
        -> SF Bool Double
powerCalc s start = switch calc (powerCalc (-s))
  where
    calc = proc input -> do
      val <- integral >>^ (+start) -< if input then s else 0
      should_switch <- edge -< val > 300 || val < 0
      returnA -< (val, should_switch `tag` val)

-- |computePrevVals updates our list of 3 recent positions
computePrevVals :: SF (Double, Double, [V2 Double]) [V2 Double]
computePrevVals = proc input@(px, py, valtabs) -> do
  returnA -< [V2 px py, head valtabs, valtabs !! 1]

-- |getVA this is the most important SF: it is concerned with velocity. Our acceleration values are modified discretely, and our position value
-- is based only on velocity. Velocity values are changed in continuous and discrete manner (it changes all the time based on acceleration,
-- and it may change according to events). Discrete changes are implemented as rSwitch in ballController, and continuous changes are
-- implemented here.
getVA :: Ball                                                                                   -- ^ Ball data to use
      -> SF (V2 Double, V2 Double, Bool) (V2 Double, V2 Double)                                 -- ^ Resulting SF counting velocity and passing acceleration
getVA b@(Ball _ v@(V2 vx vy) a@(V2 ax ay) _ _) = proc inp@(vi@(V2 vxi vyi), ai@(V2 axi ayi), rolling) -> do
            -- for now, I haven't modelled any kind of horizontal accelerations other than friction. If we're rolling and our horizontal velocity is very low,
            -- we should stop. If we're just rolling, we use given acceleration. Otherwise we have no horizontal acceleration and our X velocity doesn't
            -- change.
            speedX <- integral >>^ (+vx) -< if rolling && (abs vxi <= 1.0) then 0 else if rolling then ax else 0

            -- if we're rolling, we should not change our vertical velocity unless event happens. If we're not,
            -- we should just use gravitational acceleration.
            speedY <- integral >>^ (+vy) -< if rolling then 0 else ayi
            returnA -< (V2 (if abs speedX <= 1.0 then 0 else speedX) speedY, a)

-- |Check whether we collided with spikes
verifyLoseCondition :: SF (Event (GameObjType, HitDir)) (Event ())
verifyLoseCondition = proc input -> do
  case input of
    NoEvent -> returnA -< NoEvent
    Event (Spikes, _) -> returnA -< Event ()
    _ -> returnA -< NoEvent

-- |Check whether we collided with goal
verifyWinCondition :: SF (Event (GameObjType, HitDir)) (Event ())
verifyWinCondition = proc input -> do
  case input of
    NoEvent -> returnA -< NoEvent
    Event (Goal, _) -> returnA -< Event ()
    _ -> returnA -< NoEvent

-- |This SF filters far objects from ObjsMap given in input
filterFarObjectsSF :: GameInfo -> SF (ObjsMap, Point V2 Int) ObjsMap
filterFarObjectsSF gi = proc input@(objs, p) -> do
  returnA -< filterFarObjects gi objs p

-- |collisionCalculator processes all objects from given ObjsMap and checks every edge for a
-- collision with current player position
collisionCalculator :: (Int, Int)                                               -- ^ Tile width and height
                  -> SF (ObjsMap, Point V2 Double, Double) CollisionEffect      -- ^ SF to count CollisionEffect
collisionCalculator (width, height) = proc input@(objsMap, p, r) -> do
  returnA -< CollisionEffect {
    leftHitEffect = checkLeftHit p r objsMap,
    rightHitEffect = checkRightHit p r objsMap,
    upHitEffect = checkUpHit p r objsMap,
    downHitEffect = checkDownHit p r objsMap,
    leftUpHitEffect = checkLeftUpHit p r objsMap,
    rightUpHitEffect = checkRightUpHit p r objsMap,
    leftDownHitEffect = checkLeftDownHit p r objsMap,
    rightDownHitEffect = checkRightDownHit p r objsMap
  }
  where
    aggreg val = fmap objType $ headMaybe $ Map.elems val
    -- every helper function takes ball postion, its radius and filtered ObjsMap.
    -- It then searches for GameObj for which collision coniditions are satisfied.
    -- There might be a collection of such objects so we take only first (it doesn't matter).
    checkLeftHit :: Point V2 Double -> Double -> ObjsMap -> Maybe GameObjType
    checkLeftHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True)
                                                                      [px <= fromIntegral cpx,
                                                                      py >= fromIntegral cpy,
                                                                      py <= fromIntegral (cpy + height),
                                                                      radius >= (fromIntegral cpx - px)]) filteredObjs
    checkRightHit :: Point V2 Double -> Double -> ObjsMap -> Maybe GameObjType
    checkRightHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True)
                                                                      [px >= fromIntegral (cpx + width),
                                                                      py >= fromIntegral cpy,
                                                                      py <= fromIntegral (cpy + height),
                                                                      radius >= (px - fromIntegral (cpx + width))]) filteredObjs
    checkUpHit :: Point V2 Double -> Double -> ObjsMap -> Maybe GameObjType
    checkUpHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True)
                                                                      [py <= fromIntegral cpy,
                                                                      px >= fromIntegral cpx,
                                                                      px <= fromIntegral (cpx + width),
                                                                      radius >= (fromIntegral cpy - py)]) filteredObjs
    checkDownHit :: Point V2 Double -> Double -> ObjsMap -> Maybe GameObjType
    checkDownHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True)
                                                                      [py >= fromIntegral (cpy + height),
                                                                      px >= fromIntegral cpx,
                                                                      px <= fromIntegral (cpx + width),
                                                                      radius >= (py - fromIntegral (cpy + height))]) filteredObjs
    checkLeftUpHit :: Point V2 Double -> Double -> ObjsMap -> Maybe GameObjType
    checkLeftUpHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True)
                                                                      [py < fromIntegral cpy,
                                                                      px < fromIntegral cpx,
                                                                      radius > sqrt ((fromIntegral cpy - py)^2 + (fromIntegral cpx - px)^2)]) filteredObjs
    checkRightUpHit :: Point V2 Double -> Double -> ObjsMap -> Maybe GameObjType
    checkRightUpHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True)
                                                                      [py < fromIntegral cpy,
                                                                      px > fromIntegral (cpx + width),
                                                                      radius > sqrt ((fromIntegral cpy - py)^2 + (px - fromIntegral (cpx + width))^2)]) filteredObjs
    checkLeftDownHit :: Point V2 Double -> Double -> ObjsMap -> Maybe GameObjType
    checkLeftDownHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True)
                                                                      [py > fromIntegral (cpy + height),
                                                                      px < fromIntegral cpx,
                                                                      radius > sqrt ((py - fromIntegral (cpy + height))^2 + (fromIntegral cpx - px)^2)]) filteredObjs
    checkRightDownHit :: Point V2 Double -> Double -> ObjsMap -> Maybe GameObjType
    checkRightDownHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True)
                                                                      [py > fromIntegral (cpy + height),
                                                                      px > fromIntegral (cpx + width),
                                                                      radius > sqrt ((py - fromIntegral (cpy + height))^2 + (px - fromIntegral (cpx + width))^2)]) filteredObjs

-- |collisionChecker is responsible for generating informations on collisions. It uses 
-- collisionCalculator to calculate collisions. Beside generating event on collision,
-- it gives us continuous info whether we're rolling
collisionChecker :: GameInfo ->                                                                             
                SF (Point V2 Double, V2 Double, Double, ObjsMap) (Event (GameObjType, HitDir), Bool)        
collisionChecker gi@(GameInfo _ _ width height _ _ _) = proc input@(p, v@(V2 vx vy), r, objsMap) -> do
  filteredObjs <- filterFarObjectsSF gi -< (objsMap, fmap round p) -- we filter out far objects
  collisionEffects <- collisionCalculator (fromIntegral width, fromIntegral height) -< (filteredObjs, p, r) -- we generate CollisionEffect
  -- for each of 4 walls, we want to generate event on rising edge of wall hit
  leftHit <- edgeJust -< leftHitEffect collisionEffects
  rightHit <- edgeJust -< rightHitEffect collisionEffects
  upHit <- edgeJust -< upHitEffect collisionEffects
  downHit <- edgeJust -< downHitEffect collisionEffects
  -- for each of 4 corners, we want to SKIP event if we're already colliding with a wall or rolling.
  -- otherwise, we generate event on rising edge
  leftUpHit <- edgeJust -< if sideHit collisionEffects || rolling collisionEffects vy then Nothing else leftUpHitEffect collisionEffects
  rightUpHit <- edgeJust -< if sideHit collisionEffects || rolling collisionEffects vy then Nothing else rightUpHitEffect collisionEffects
  leftDownHit <- edgeJust -< if sideHit collisionEffects || rolling collisionEffects vy then Nothing else leftDownHitEffect collisionEffects
  rightDownHit <- edgeJust -< if sideHit collisionEffects || rolling collisionEffects vy then Nothing else rightDownHitEffect collisionEffects
  -- we are said to be "rolling" when we're colliding with upper part of collider and our vertical velocity is very low
  returnA -< (mergeEvents [upHit `attach` UpSide, downHit `attach` DownSide,
                          rightHit `attach` RightSide, leftHit `attach` LeftSide,
                          leftUpHit `attach` LeftUpSide, rightUpHit `attach` RightUpSide,
                          leftDownHit `attach` LeftDownSide, rightDownHit `attach` RightDownSide],
                          rolling collisionEffects vy)
  where
    sideHit :: CollisionEffect -> Bool
    sideHit colls = any isJust [leftHitEffect colls, rightHitEffect colls, upHitEffect colls, downHitEffect colls]

    rolling :: CollisionEffect -> Double -> Bool
    rolling collisionEffects vy = 
      ((isJust $ upHitEffect collisionEffects) || (isJust $ leftUpHitEffect collisionEffects) || (isJust $ rightUpHitEffect collisionEffects)) && (abs vy <= 10)