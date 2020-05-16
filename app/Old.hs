{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
module Old where

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

import Parser
import Types

initScreenWidth, initScreenHeight :: CInt
(initScreenWidth, initScreenHeight) = (1280, 960)

initTileWidth, initTileHeight :: CInt
(initTileWidth, initTileHeight) = (32, 32)

downWithTheBall :: Ball -> BallSF
downWithTheBall b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) = proc gi -> do
  velocityX <- integral >>^ (+vx) -< ax
  velocityY <- integral >>^ (+vy) -< ay
  positionX <- (integral >>^ (+px)) -< velocityX
  positionY <- (integral >>^ (+py)) -< velocityY
  returnA -< Ball {
              position = P (V2 positionX positionY),
              velocity = V2 velocityX velocityY,
              acceleration = V2 ax ay,
              radius = r
            }

downWithTheBallTest :: Ball -> BallSF
downWithTheBallTest b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) = proc gi -> do
  velocityX <- integral >>^ (+vx) -< ax
  velocityY <- integral >>^ (+vy) -< ay
  returnA -< Ball {
              position = P (V2 velocityX velocityY),
              velocity = V2 velocityX velocityY,
              acceleration = V2 ax ay,
              radius = r
            }

collisionSwitch :: Ball -> BallSF
collisionSwitch b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) = switch
                                                                          (collisionCheckMove b)
                                                                          (\ball@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) ->
                                                                          -- if vy >= -1 && vy <= 1 then stationarySwitch ball else (clickSwitch . modifyBall) ball)
                                                                          -- (clickSwitch . modifyBall) ball)
                                                                          (downWithTheBallTest . modifyBall) ball)
  where collisionCheckMove b@(Ball p v a r) = proc input -> do
                        b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) <- downWithTheBall b -< input
                        event <- edge -< py >= fromIntegral (initScreenHeight - initTileHeight)
                        returnA -< (b, event `tag` b)
        modifyBall b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) = Ball {
                                                                              position = p,
                                                                              velocity = V2 vx (0.6 * (-vy)),
                                                                              acceleration = a,
                                                                              radius = r
                                                                            }

clickSwitch :: Ball -> BallSF
clickSwitch b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) = switch (clickCheckMove b) (clickSwitch . modifyBall)
  where clickCheckMove b@(Ball p v a r) = proc input -> do
                        ball <- collisionSwitch b -< input
                        isClicked <- mouseClickParser -< input
                        event <- edge -< isEvent isClicked
                        P (V2 mX mY) <- mousePosParser  -< input
                        returnA -< (ball, event `tag` ball)
        modifyBall b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) = Ball {
                                                                              position = p,
                                                                              velocity = V2 vx (vy - 15),
                                                                              acceleration = a,
                                                                              radius = r
                                                                            }


-- vectorApply f v = vector2 (f $ vector2X v) (f $ vector2Y v) (f $ vector3Z v)

integral' = (iPre 0.0 &&& time) >>> sscan f (0.0, 0) >>> arr fst
    where f (!prevVal, !prevTime) (!val, !time)
            | val == 0.0 =
                (0.0, time)
            | otherwise         =
                (prevVal + realToFrac (time - prevTime) * val, time)

stationarySwitch :: Ball -> BallSF
stationarySwitch b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) = constant b
  -- switch (clickStationary b) (clickSwitch . modifyBall)
  -- where clickStationary b@(Ball p v a r) = proc input -> do
  --                       ballik <- constant b -< input
  --                       isClicked <- mouseClickParser -< input
  --                       event <- edge -< isEvent isClicked
  --                       P (V2 mX mY) <- mousePosParser  -< input
  --                       returnA -< (ballik, event `tag` ballik)
  --       modifyBall b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) = Ball {
  --                                                                             position = P (V2 px (py-1)),
  --                                                                             velocity = V2 vx (-20),
  --                                                                             acceleration = a,
  --                                                                             radius = r
  --                                                                           }