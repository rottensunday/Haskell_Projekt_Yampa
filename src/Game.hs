{-|
Module      : Game
Description : Game main and SDL loops
-}
module Game where

import FRP.Yampa
import Linear
import Linear.Metric
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import Data.Text(Text(..), pack)
import qualified Data.Map.Strict as Map
import qualified SDL
import SDL (($=), Point(..), Rectangle)
import qualified SDL.Font as SFont
import Control.Applicative
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO
import System.Directory
import qualified Data.List as L

import ObjectsLoader
import SDLWrapper
import Parser
import Types
import GameController
import Constants

-- |Main is supposed to initialize all IO (create SDL Window, create Renderer, load maps and scores from files) and then start reactimate loop
main :: IO ()
main = do
  SDL.initializeAll
  SFont.initialize
  font <- SFont.load fontPath 15
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear

  window <-
    SDL.createWindow
      (pack windowName)
      SDL.defaultWindow {SDL.windowInitialSize = V2 initScreenWidth initScreenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.SoftwareRenderer
        , SDL.rendererTargetTexture = False
        }
  SDL.windowGrab window $= True
  spriteSheetTexture <- loadTexture renderer spriteSheetPath
  staticObjs <- prepareStaticObjsMap (initTileWidth, initTileHeight) mapsPath
  dynamicObjs <- prepareDynamicObjs $ length staticObjs
  scoresFilesPaths <- prepareScoresFiles mapsPath
  scores <- mapM (\filePath -> withFile filePath ReadMode (many . hGetLine) >>= (return . take 3 . L.sort . map (read :: String -> Int))) scoresFilesPaths
  reactimate parseInput
    (\_ -> parseInput >>= (\gi -> return (simulationRate, Just gi))) -- we parse input (using SDL) and give it to the controller
    (\_ output -> appLoop renderer spriteSheetTexture scores scoresFilesPaths staticObjs font output) -- given GameController output, we go into SDL loop
    (gameController startBall GameInfo {
                              screenWidth = initScreenWidth,
                              screenHeight = initScreenHeight,
                              tileWidth = initTileWidth,
                              tileHeight = initTileHeight,
                              staticObjsMap = staticObjs,
                              dynamicObjsInfo = dynamicObjs,
                              currLvl = 0
                              }
    )

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

-- |appLoop is responsible for displaying game state with SDL and managing IO when needed (for example: saving score)
appLoop :: SDL.Renderer   -- ^ Renderer used
          -> Texture      -- ^ Sprite sheet
          -> [[Int]]      -- ^ Highscores for each level
          -> [FilePath]   -- ^ Filepaths for scores to write to
          -> [ObjsMap]    -- ^ Map of game objects with positions as keys
          -> SFont.Font   -- ^ Font used to write game info on screen
          -> GameOutput   -- ^ Yampa ouput which gives us gamestate info
          -> IO Bool      -- ^ Should we finish our loop?
appLoop renderer sheet scores scoresPaths objsMap font go@(GameOutput b@(Ball p@(P pV2@(V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r pow) end nshots didwin didfinish lvl objs outtime) = do
  mousePos@(P mV2) <- SDL.getAbsoluteMouseLocation -- we'll need mouse position to draw Power line
  let textColor = V4 255 0 0 0
  SDL.rendererDrawColor renderer $= V4 0 0 0 0
  SDL.clear renderer
  renderTexture renderer sheet (SDL.P (V2 (round (px-(r+2))) (round (py-(r+2))))) (Just ballClip) -- draw Ball
  drawObjs -- draw game objects
  SDL.rendererDrawColor renderer $= V4 255 255 255 255
  SDL.drawLine renderer (P (fmap round startDir)) (P (fmap round startDir + lineEnd mV2)) -- draw white line of Power
  shotsText <- loadTextTexture renderer font ("Number of shots:  " ++ show nshots) textColor
  lvlText <- loadTextTexture renderer font ("Level:  " ++ show lvl) textColor
  scoresText <- loadTextTexture renderer font ("High scores: " ++ show (scores !! lvl)) textColor
  renderTexture renderer shotsText (P (V2 100 100)) Nothing
  renderTexture renderer lvlText (P (V2 100 150)) Nothing
  renderTexture renderer scoresText (P (V2 100 200)) Nothing
  SDL.destroyTexture $ texture shotsText
  SDL.destroyTexture $ texture lvlText
  SDL.destroyTexture $ texture scoresText
  if didwin then appendFile (scoresPaths !! lvl) (show nshots ++ "\n") else pure () -- if we finished level: append score to file
  if didfinish
    then do
      SDL.showSimpleMessageBox Nothing SDL.Information (pack "Gra pokonana!") (pack "Gratulacje! Przeszedłeś wszystkie poziomy.")
      return True
    else do
      SDL.present renderer
      time <- round . (*1000000) <$> getPOSIXTime
      threadDelay (2000 - fromIntegral (time-outtime)) -- game is supposed to work in 500 fps. if our frame did not last for
                                                       -- 2 miliseconds, we shall wait. 
      return end

    where
      startDir = pV2
      dir :: V2 CInt -> V2 Double
      dir mV2 = Linear.Metric.normalize (fmap fromIntegral mV2 - pV2)
      lineEnd :: V2 CInt -> V2 CInt
      lineEnd mV2 = round <$> V2 pow pow * dir mV2
      drawObjs =  sequence_ $ Map.foldlWithKey
                            (\col (posx, posy) gameobj -> renderTexture renderer sheet (SDL.P (V2 (fromIntegral posx :: CInt) (fromIntegral posy :: CInt))) (Just $ textureCoords gameobj):col)
                            []
                            objs