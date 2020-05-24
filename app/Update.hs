{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
module Update where

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
import Control.Lens

import Parser
import Types
import Utility

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe x = Just $ head x

-- our clicks should only modivy velocity, based on current position and mouse position
clickSpeedModifier :: SF (Event (Point V2 Double, Point V2 Double, V2 Double, V2 Double, V2 Double, Bool, Double)) (V2 Double)
clickSpeedModifier = proc input -> do
  case input of
    NoEvent -> returnA -< 0
    Event ((P mPos@(V2 mPosX mPosY)), (P pPos@(V2 pPosX pPosY)), v@(V2 vX vY), a@(V2 aX aY), aStart@(V2 aStartX aStartY), rolling, pow) ->
      returnA -< velChange
        where
          dir@(V2 vx vy) = Linear.Metric.normalize (mPos - pPos) -- determine direction of movement
          velChange = (V2 pow pow) * dir -- our new velocity vector has length of 50 in direction of mouse position

-- collisions may affect both velocity and acceleration (for example, when we're rolling and friction works. When we hit a wall,
-- friction direction should change.) 
hitSAModifier2 :: SF (Event ((GameObjType, HitDir), (Point V2 Double, Point V2 Double, V2 Double, V2 Double, V2 Double, Bool))) (V2 Double, V2 Double)
hitSAModifier2 = proc input -> do
  case input of
    NoEvent -> returnA -< (V2 0 0, V2 0 0)
    Event ((Wall, UpSide), ((P mPos@(V2 mPosX mPosY)), (P pPos@(V2 pPosX pPosY)), v@(V2 vX vY), a@(V2 aX aY), aStart@(V2 aStartX aStartY), rolling)) ->
      -- if we hit a ground and our velocity is very low, we should stop. If we hit a ground and are now rolling, we should apply friction.
      returnA -< (V2 vX (if (abs vY <= 10) then 0 else if vY > 0 then vY * (-0.6) else vY), if rolling then V2 (calculateXAcc vX) aY else a)
    Event ((Wall, DownSide), ((P mPos@(V2 mPosX mPosY)), (P pPos@(V2 pPosX pPosY)), v@(V2 vX vY), a@(V2 aX aY), aStart@(V2 aStartX aStartY), rolling)) ->
      returnA -< (V2 vX (vY * (-0.6)), a)
    Event ((Wall, LeftSide), ((P mPos@(V2 mPosX mPosY)), (P pPos@(V2 pPosX pPosY)), v@(V2 vX vY), a@(V2 aX aY), aStart@(V2 aStartX aStartY), rolling)) ->
      -- hitting left or right wall should change direction of X acceleration. This is used only for friction. Addition of other horizontal forces
      -- may change this.
      returnA -< (V2 (vX * (-0.6)) vY, V2 (-aX) aY)
    Event ((Wall, RightSide), ((P mPos@(V2 mPosX mPosY)), (P pPos@(V2 pPosX pPosY)), v@(V2 vX vY), a@(V2 aX aY), aStart@(V2 aStartX aStartY), rolling)) ->
      returnA -< (V2 (vX * (-0.6)) vY, V2 (-aX) aY)
    Event ((Wall, LeftDownSide), ((P mPos@(V2 mPosX mPosY)), (P pPos@(V2 pPosX pPosY)), v@(V2 vX vY), a@(V2 aX aY), aStart@(V2 aStartX aStartY), rolling)) ->
      returnA -< if vY >= 0 || rolling || ((abs vY) <= 10) then (v, a) else (V2 (-vX) (vY * (-0.6)), a)
      --returnA -< (v, a)
    Event ((Wall, RightDownSide), ((P mPos@(V2 mPosX mPosY)), (P pPos@(V2 pPosX pPosY)), v@(V2 vX vY), a@(V2 aX aY), aStart@(V2 aStartX aStartY), rolling)) ->
      returnA -< if vY >= 0 || rolling || ((abs vY) <= 10) then (v, a) else (V2 (-vX) (vY * (-0.6)), a)
      --returnA -< (v, a)
    Event ((Wall, LeftUpSide), ((P mPos@(V2 mPosX mPosY)), (P pPos@(V2 pPosX pPosY)), v@(V2 vX vY), a@(V2 aX aY), aStart@(V2 aStartX aStartY), rolling)) ->
      returnA -< if vY <= 0 || rolling || ((abs vY) <= 10) then (v, a) else (V2 (-vX) (vY * (-0.6)), a)
      --returnA -< (v, a)
    Event ((Wall, RightUpSide), ((P mPos@(V2 mPosX mPosY)), (P pPos@(V2 pPosX pPosY)), v@(V2 vX vY), a@(V2 aX aY), aStart@(V2 aStartX aStartY), rolling)) ->
      returnA -< if vY <= 0 || rolling || ((abs vY) <= 10) then (v, a) else (V2 (-vX) (vY * (-0.6)), a)
      --returnA -< (v, a)
  where calculateXAcc vX
          | vX <= 0   = 1.5
          | otherwise = (-1.5)

-- this SF is supposed to calculate power of ball shot.
-- it just applies acc value to integral and switches, so we go back and forward
-- between 0 and 100 power value
powerCalc :: Double -> Double -> SF Bool Double
powerCalc acc start = switch (calc) (\cont -> powerCalc (-acc) cont)
  where
    calc = proc input -> do
      rec should_switch <- edge -< val > 300 || val < 0
          val <- iPre 0 >>> integral >>^ (+start) -< if input then acc else 0
      returnA -< (val, should_switch `tag` val)

ballController :: Ball -> GameInfo -> BallSF
ballController b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r pow)
             gi@(GameInfo sWidth sHeight tWidth tHeight objs) = switch (controller) (\_ -> (ballController b gi))
              where
                controller :: SF GameInput (GameOutput, Event ())
                controller = proc input -> do
                  lmbReleased <- mouseEventReleasedParser -< input -- determine whether we released LMButton
                  nClicks <- accumHold 0 -< lmbReleased `tag` (\val -> val+1)
                  qClicked <- qClickParser >>> arr (isEvent) -< input
                  -- we have to use delayed switch so jumpPower isn't set to 0 after LMB release. If this happens, we can't really use it to shoot our ball.
                  jumpPower <- drSwitch (mousePressedParser >>> powerCalc 60 0) -< (input, lmbReleased `tag` (mousePressedParser >>> powerCalc 60 0))
                  mPos <- mousePosParser -< input -- determine mouse position

                  rec velModClick <- clickSpeedModifier -< (lmbReleased `tag` (mPos, P (V2 posX posY), speed, acc, a, rolling, jumpPower)) -- clicking a button changes our velocity
                      -- hitting an edge may change our velocity and acceleration. If we hit a floor and our velocity is very low, we start rolling on the ground.
                      -- (isGroundHit, rolling) <- fby (NoEvent, False) edgeHitCheck -< (V2 posX posY, speed, rolling, gi) 
                      (collisionEvent, rolling) <- fby (NoEvent, False) (collisionChecker objs) -< (P (V2 posX posY), speed, r, rolling)
                      -- hitting an edge may change our velocity and acceleration
                      -- velModHit <- hitSAModifier -< (isGroundHit `attach` (mPos, P (V2 posX posY), speed, acc, a, rolling))
                      velModHit <- hitSAModifier2 -< (collisionEvent `attach` (mPos, P (V2 posX posY), speed, acc, a, rolling))
                      -- every time we have click or edge hit event, we switch our speed-determining SF to a new one,
                      -- so it can make its calculation based on new parameters. We provide a parameter for this SF with a "getTag" function,
                      -- which processes click/edge hit events modified values and makes a new ball.
                      (speed@(V2 speedX speedY), acc@(V2 accX accY)) <- fby (V2 0.0 0.0, V2 0.0 0.0) (rSwitch (getVA b)) -< ((input, P (V2 posX posY), speed, acc, rolling),
                                                                                              lMerge lmbReleased (collisionEvent `tag` ()) `tag` getVA (getTag lmbReleased (collisionEvent `tag` ()) velModClick velModHit (P (V2 posX posY)) speed acc r rolling))
                      posX <- rSwitch (iPre px >>> integral >>^ (+px)) -< (speedX, collisionEvent `tag` (integral >>^ (+((valTabs !! 2) ^. _x))))
                      posY <- rSwitch (iPre py >>> integral >>^ (+py)) -< (speedY, collisionEvent `tag` (integral >>^ (+((valTabs !! 2) ^. _y))))
                      eventLose <- verifyLoseCondition -< collisionEvent
                      eventWin <- verifyWinCondition -< collisionEvent
                      valTabs <- fby ([V2 px py, V2 px py, V2 px py]) (computeValTabs) -< (posX, posY, valTabs)

                  
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
                              didWin = isEvent eventWin
                          }, eventLose)
                -- getTag (clickEvent) (hitEvent) (clickEventModifiers) (hitEventModifiers) (position) (speed) (acceleration) (radius) (isRolling)
                getTag :: Event () -> Event () -> V2 Double -> (V2 Double, V2 Double) -> Point V2 Double -> V2 Double -> V2 Double -> Double -> Bool -> Ball
                getTag NoEvent NoEvent _ _ _ _ _ _ _ = undefined -- this cannot be called
                getTag (Event ()) NoEvent modC@(V2 vx vy) modH p v a r rolling = Ball { -- after click
                                            position = p,
                                            velocity = if rolling && vy > 0 then (V2 vx 0) else modC,
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

computeValTabs :: SF (Double, Double, [V2 Double]) [V2 Double]
computeValTabs = proc input@(px, py, valtabs) -> do
  returnA -< [V2 px py, valtabs !! 0, valtabs !! 1]

-- this is the most important SF: it is concerned with velocity. Our acceleration values are modified discretely, and our position value
-- is based only on velocity. Velocity values are changed in continuous and discrete manner (it changes all the time based on acceleration,
-- and it may change according to events). Discrete changes are implemented as rSwitch in ballController, and continuous changes are
-- implemented here.
getVA :: Ball -> HardSF
getVA b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r pow) = proc inp@(ginp, pi@(P (V2 pxi pyi)), vi@(V2 vxi vyi), ai@(V2 axi ayi), rolling) -> do
            -- for now, I haven't modelled any kind of horizontal accelerations other than friction. If we're rolling and our horizontal velocity is very low,
            -- we should stop. If we're just rolling, we use given acceleration. Otherwise we have no horizontal acceleration and our X velocity doesn't
            -- change.
            speedX <- integral >>^ (+vx) -< if rolling && (abs vxi <= 1.0) then 0 else if rolling then ax else 0
            -- if we're rolling, we should not change our vertical velocity unless event happens. If we're not,
            -- we should just use gravitational acceleration.
            speedY <- integral >>^ (+vy) -< if rolling then 0 else ayi

            returnA -< (V2 (if abs speedX <= 1.0 then 0 else speedX) speedY, a)

edgeHitCheck :: SF (V2 Double, V2 Double, Bool, GameInfo) (Event HitDir, Bool)
edgeHitCheck = proc input@(V2 px py, v@(V2 vx vy), rolling, gi) -> do
  eventDown <- edgeTag DownSide -< groundCollisionCheck py gi
  eventUp <- edgeTag UpSide -< py <= 0
  eventLeft <- edgeTag LeftSide -< px <= 0
  eventRight <- edgeTag RightSide -< rightEdgeCollisionCheck px gi
  returnA -< (mergeEvents [eventUp, eventLeft, eventRight, eventDown], checkRolling (groundCollisionCheck py gi) v)
  where
    groundCollisionCheck :: Double -> GameInfo -> Bool
    groundCollisionCheck py gi = py >= fromIntegral (screenHeight gi - tileHeight gi)
    rightEdgeCollisionCheck :: Double -> GameInfo -> Bool
    rightEdgeCollisionCheck px gi = px >= fromIntegral (screenWidth gi - tileWidth gi)
    checkRolling :: Bool -> V2 Double -> Bool
    checkRolling cond v@(V2 vx vy) = if cond && (abs vy <= 10.0) then True else False

verifyLoseCondition :: SF (Event (GameObjType, HitDir)) (Event ())
verifyLoseCondition = proc input -> do
  case input of
    NoEvent -> returnA -< NoEvent
    Event (Spikes, _) -> returnA -< Event ()
    _ -> returnA -< NoEvent
  
verifyWinCondition :: SF (Event (GameObjType, HitDir)) (Event ())
verifyWinCondition = proc input -> do
  case input of
    NoEvent -> returnA -< NoEvent
    Event (Goal, _) -> returnA -< Event ()
    _ -> returnA -< NoEvent

filterFarObjectsSF :: SF (StaticObjsMap, Point V2 Int) StaticObjsMap
filterFarObjectsSF = proc input@(objs, p) -> do
  returnA -< filterFarObjects objs p

collisionCalculator :: SF (StaticObjsMap, Point V2 Double, Double) CollisionEffect
collisionCalculator = proc input@(objsMap, p, r) -> do
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
    checkLeftHit :: Point V2 Double -> Double -> StaticObjsMap -> Maybe GameObjType
    checkLeftHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True) 
                                                                      [px <= fromIntegral cpx*32,
                                                                      py >= fromIntegral cpy*32,
                                                                      py <= fromIntegral (cpy*32 + 32),
                                                                      radius >= (fromIntegral cpx*32 - px)]) filteredObjs
    checkRightHit :: Point V2 Double -> Double -> StaticObjsMap -> Maybe GameObjType
    checkRightHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True) 
                                                                      [px >= fromIntegral (cpx*32 + 32),
                                                                      py >= fromIntegral cpy*32,
                                                                      py <= fromIntegral (cpy*32 + 32),
                                                                      radius >= (px - fromIntegral (cpx*32 + 32))]) filteredObjs
    checkUpHit :: Point V2 Double -> Double -> StaticObjsMap -> Maybe GameObjType
    checkUpHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True) 
                                                                      [py <= fromIntegral cpy*32,
                                                                      px >= fromIntegral cpx*32,
                                                                      px <= fromIntegral (cpx*32 + 32),
                                                                      radius >= (fromIntegral cpy*32 - py)]) filteredObjs
    checkDownHit :: Point V2 Double -> Double -> StaticObjsMap -> Maybe GameObjType
    checkDownHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True) 
                                                                      [py >= fromIntegral (cpy*32 + 32),
                                                                      px >= fromIntegral cpx*32,
                                                                      px <= fromIntegral (cpx*32 + 32),
                                                                      radius >= (py - fromIntegral (cpy*32 + 32))]) filteredObjs
    checkLeftUpHit :: Point V2 Double -> Double -> StaticObjsMap -> Maybe GameObjType
    checkLeftUpHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True) 
                                                                      [py < fromIntegral cpy*32,
                                                                      px < fromIntegral cpx*32,
                                                                      radius > sqrt ((fromIntegral cpy*32 - py)^2 + (fromIntegral cpx*32 - px)^2)]) filteredObjs
    checkRightUpHit :: Point V2 Double -> Double -> StaticObjsMap -> Maybe GameObjType
    checkRightUpHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True) 
                                                                      [py < fromIntegral cpy*32,
                                                                      px > fromIntegral (cpx*32 + 32),
                                                                      radius > sqrt ((fromIntegral cpy*32 - py)^2 + (px - fromIntegral (cpx*32 + 32))^2)]) filteredObjs
    checkLeftDownHit :: Point V2 Double -> Double -> StaticObjsMap -> Maybe GameObjType
    checkLeftDownHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True) 
                                                                      [py > fromIntegral (cpy*32 + 32),
                                                                      px < fromIntegral cpx*32,
                                                                      radius > sqrt ((py - fromIntegral (cpy*32 + 32))^2 + (fromIntegral cpx*32 - px)^2)]) filteredObjs
    checkRightDownHit :: Point V2 Double -> Double -> StaticObjsMap -> Maybe GameObjType
    checkRightDownHit p@(P (V2 px py)) radius filteredObjs = aggreg $ Map.filterWithKey (\(cpx :: Int, cpy :: Int) go -> all (== True) 
                                                                      [py > fromIntegral (cpy*32 + 32),
                                                                      px > fromIntegral (cpx*32 + 32),
                                                                      radius > sqrt ((py - fromIntegral (cpy*32 + 32))^2 + (px - fromIntegral (cpx*32 + 32))^2)]) filteredObjs

collisionChecker :: StaticObjsMap -> SF (Point V2 Double, V2 Double, Double, Bool) (Event (GameObjType, HitDir), Bool)
collisionChecker objsMap = proc input@(p@(P (V2 px py)), v@(V2 vx vy), r, rolling) -> do
  filteredObjs <- filterFarObjectsSF -< (objsMap, fmap round p)
  collisionEffects <- collisionCalculator -< (filteredObjs, p, r) 
  leftHit <- edgeJust -< leftHitEffect collisionEffects
  rightHit <- edgeJust -< rightHitEffect collisionEffects
  upHit <- edgeJust -< upHitEffect collisionEffects
  downHit <- edgeJust -< downHitEffect collisionEffects
  leftUpHit <- edgeJust -< if sideHit collisionEffects || rolling then Nothing else leftUpHitEffect collisionEffects
  rightUpHit <- edgeJust -< if sideHit collisionEffects || rolling then Nothing else rightUpHitEffect collisionEffects
  leftDownHit <- edgeJust -< if sideHit collisionEffects || rolling then Nothing else leftDownHitEffect collisionEffects
  rightDownHit <- edgeJust -< if sideHit collisionEffects || rolling then Nothing else rightDownHitEffect collisionEffects
  returnA -< (mergeEvents [upHit `attach` UpSide, downHit `attach` DownSide,
                          rightHit `attach` RightSide, leftHit `attach` LeftSide,
                          leftUpHit `attach` LeftUpSide, rightUpHit `attach` RightUpSide,
                          leftDownHit `attach` LeftDownSide, rightDownHit `attach` RightDownSide], 
                          ((isJust $ upHitEffect collisionEffects) || (isJust $ leftUpHitEffect collisionEffects) || (isJust $ rightUpHitEffect collisionEffects)) && (abs vy <= 10))
  where 
    sideHit :: CollisionEffect -> Bool
    sideHit colls = any isJust [leftHitEffect colls, rightHitEffect colls, upHitEffect colls, downHitEffect colls]
  