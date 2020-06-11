{-|
Module      : GameController
Description : Signal function to control game state
-}
{-# LANGUAGE Arrows #-}
module GameController where

import FRP.Yampa
import Linear
import Linear.Affine
import qualified Data.Map as Map
import FRP.Yampa.Switches

import Parser
import Types
import BallController

-- |This simple controller is used to control the state of dynamic game objects beside the player.
-- These objects have very basic functionanlity: initial position, positon limit, directory and speed.
-- They just slide back and forward.
dynamicObjectController :: DynamicGameObj               -- ^ Our object data
                        -> Double                       -- ^ Position value to add to initial position (used in switching)
                        -> SF () (V2 Double, GameObj)   -- ^ SF which controls our object position. It returns data necessary for Ball to consider (position and type)
dynamicObjectController dgo@(DynamicGameObj obj pos@(P (V2 px py)) dir s l) start = switch calc (dynamicObjectController (DynamicGameObj obj pos dir (-s) l))
  where
    -- we only consider the direction and then modify corresponding position based on speed
    -- switching is used to "bounce" when achieving position limit
    calc = case dir of VerticalUp -> vertCalcUp
                       VerticalDown -> vertCalcDown
                       HorizontalLeft -> horCalcLeft
                       HorizontalRight -> horCalcLeft
    vertCalcUp = proc input -> do
      val <- integral >>^ (+start) -< s
      should_switch <- edge -< val < (py-l) || val > py
      returnA -< ((V2 px val, obj), should_switch `tag` val)
    vertCalcDown = proc input -> do
      val <- integral >>^ (+start) -< s
      should_switch <- edge -< val > (py+l) || val < py
      returnA -< ((V2 px val, obj), should_switch `tag` val)
    horCalcLeft = proc input -> do
      val <- integral >>^ (+start) -< s
      should_switch <- edge -< val < (px-l) || val > px
      returnA -< ((V2 val py, obj), should_switch `tag` val)
    horCalcRight = proc input -> do
      val <- integral >>^ (+start) -< s
      should_switch <- edge -< val > (px+l) || val < px
      returnA -< ((V2 val py, obj), should_switch `tag` val)

-- |This controller is controlling state of the whole game. For now it just gets position of all
-- dynamic objects and uses ballController with given objects as an input to produce output
gameController :: Ball          -- ^ Initial ball state
                -> GameInfo     -- ^ Initial game info
                -> GameSF       -- ^ Signal function from GameInput to GameOutput
-- we switch on level change
gameController b gi@(GameInfo sWidth sHeight tWidth tHeight objs dyn lvl) = dSwitch gc (\_ -> gameController b gi {currLvl = lvl+1})
  where
    gc = proc input -> do
      dynamicObjectsList <- parZ prepareSFs -< replicate (length (dyn !! lvl)) () -- use dynamicObjectController for all dynamic objects
      -- in next line we use ballController with game input and we connect static objects with dynamic objects to create one big map
      output <- ballController b gi -< (input, 
                          Map.union (objs !! lvl) $ Map.fromList (map (\o@(V2 x y, obj) -> ((round x :: Int, round y :: Int), obj)) dynamicObjectsList))
      won <- edge -< didWin output
      returnA -< (considerGameFinished output, won)

      where
        -- |Prepare dynamicObjectController for every dynamic object there is
        prepareSFs :: [SF () (V2 Double, GameObj)]
        prepareSFs = map (`dynamicObjectController` 0) (dyn !! lvl)

        -- |Check whether we finished the whole game and modify GameOutput
        considerGameFinished :: GameOutput -> GameOutput
        considerGameFinished outin = if didWin outin && (lvl+1) == length objs then outin {didFinishGame = True, outLvl = lvl} else outin {outLvl = lvl}